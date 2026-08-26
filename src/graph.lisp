;;;; graph.lisp --- property-graph storage over the KV store
;;;;
;;;; The property graph is a pure view over the existing key/value
;;;; store (plan section 8, axioms D1-D4).  Within a database, the
;;;; physical layout is:
;;;;
;;;;   n:<eid>                 node record (binary-coded map)
;;;;   r:<eid>                 relationship record (type, start, end, props)
;;;;   nl:<label>:<eid>        node label index entry (presence key)
;;;;   e:<eid>:o:<type>:<rid>  out-adjacency entry (presence key)
;;;;   e:<eid>:i:<type>:<rid>  in-adjacency entry  (presence key)
;;;;   b:<eid>:<prop>          spilled binary blob (properties > inline limit)
;;;;   g:next                  id counter
;;;;
;;;; All keys are additionally namespaced with the database prefix by
;;;; the graph-view (see db.lisp), so every database holds its own
;;;; graph and every graph key shards on the existing ring.
;;;;
;;;; Axioms enforced here (plan section 4.1): A1 - every relationship
;;;; has existing start and end nodes and exactly one type; A2 - element
;;;; identity is the storage key; D2 - indexes are derived data and can
;;;; be rebuilt by a pure fold over the store (graph-rebuild-indexes).

(in-package #:scalaxy)

(defparameter +blob-inline-limit+ 1024
  "Binary property values larger than this many bytes are spilled to
their own storage key (b:<eid>:<prop>) instead of being inlined in the
element record.")

;;; ------------------------------------------------------------------
;;; graph views (storage backends)

(defclass graph-view () ()
  (:documentation "Abstract access to one database's graph."))

(defclass local-graph-view (graph-view)
  ((store :initarg :store :reader graph-store)
   (db    :initarg :db :initform "default" :reader graph-db)
   (mint-lock :initform (%make-mint-lock) :reader graph-mint-lock)
   (node-index :initform (make-hash-table :test #'equal))
   (rel-index  :initform (make-hash-table :test #'equal))
   (label-index :initform (make-hash-table :test #'equal))
   (adj-out :initform (make-hash-table :test #'equal))
   (adj-in  :initform (make-hash-table :test #'equal)))
  (:documentation "Graph view over a local STORE (in-process)."))

(defclass gateway-graph-view (graph-view)
  ((gateway :initarg :gateway :reader graph-gateway)
   (db      :initarg :db :initform "default" :reader graph-db))
  (:documentation "Graph view routed through a cluster GATEWAY."))

(defun %make-mint-lock ()
  #+sbcl (sb-thread:make-mutex :name "scalaxy-graph-mint")
  #-sbcl nil)

(defun make-local-graph (store &key (db +default-db+))
  "Graph view over the local STORE for database DB.  Derived indexes are
rebuilt from the store contents."
  (let ((g (make-instance 'local-graph-view :store store :db db)))
    (unless (store-backend store)
      (graph-rebuild-indexes g))
    g))

(defun make-gateway-graph (gateway &key (db +default-db+))
  "Graph view over the cluster through GATEWAY for database DB."
  (make-instance 'gateway-graph-view :gateway gateway :db db))

;;; ------------------------------------------------------------------
;;; storage access protocol

(defgeneric g-put (g key value)
  (:documentation "Store KEY -> VALUE (octets) in the view's database."))

(defgeneric g-get (g key)
  (:documentation "Fetch VALUE (octets) for KEY, or NIL."))

(defgeneric g-delete (g key)
  (:documentation "Delete KEY.  Returns T when it was present."))

(defgeneric g-scan (g prefix)
  (:documentation "All (logical-key . value) pairs whose key starts with PREFIX."))

(defgeneric g-counter (g key)
  (:documentation "Atomically increment the counter at KEY; return the new value."))

(defmethod g-put ((g local-graph-view) key value)
  (store-put (graph-store g) (db-key (graph-db g) key) value))

(defmethod g-get ((g local-graph-view) key)
  (store-get (graph-store g) (db-key (graph-db g) key)))

(defmethod g-delete ((g local-graph-view) key)
  (store-delete (graph-store g) (db-key (graph-db g) key)))

(defmethod g-scan ((g local-graph-view) prefix)
  (loop for (pkey . value) in (store-scan (graph-store g) (db-key (graph-db g) prefix))
        collect (cons (db-strip (graph-db g) pkey) value)))
(defgeneric g-map (g fn)
  (:documentation "Visit graph key/value pairs without allocating a sorted scan list."))
(defmethod g-map ((g local-graph-view) fn)
  (store-map (graph-store g)
             (lambda (key value)
               (funcall fn (cons (db-strip (graph-db g) key) value)))))
(defmethod g-map ((g graph-view) fn)
  (dolist (p (g-scan g "")) (funcall fn p)))

(defmethod g-map ((g gateway-graph-view) fn)
  "Stream bounded pages of primary-owned pairs without merging a
cluster-wide result list."
  (let ((gw (graph-gateway g)) (prefix (db-key (graph-db g) ""))
        (page-size 20000))
    (dolist (peer (gateway-peers gw))
      (let ((id (car peer)) (offset 0) (continue t))
        (loop while continue
              do (let ((reply (ignore-errors
                                (gateway-request
                                 gw id (list :op #.+op-scan-page+
                                             :prefix prefix :offset offset
                                             :limit page-size)))))
                   (if (and reply (eql (getf reply :status) #.+status-ok+))
                       (let ((pairs (getf reply :pairs)))
                         (dolist (p pairs)
                           (when (equal id (ring-lookup (gateway-ring gw) (car p)))
                             (funcall fn (cons (db-strip (graph-db g) (car p))
                                               (cdr p)))))
                         (incf offset (length pairs))
                         (when (< (length pairs) page-size) (setf continue nil)))
                       (setf continue nil))))))))

(defmethod g-counter ((g local-graph-view) key)
  (macrolet ((with-lock (() &body body)
               #+sbcl `(sb-thread:with-mutex ((graph-mint-lock g)) ,@body)
               #-sbcl `(progn ,@body)))
    (with-lock ()
      (let* ((cur (let ((v (g-get g key))) (if v (read-u64 v 0) 0)))
             (next (1+ cur))
             (buf (make-buffer)))
        (buf-write-u64 buf next)
        (g-put g key buf)
        next))))

(defmethod g-put ((g gateway-graph-view) key value)
  (gateway-put (graph-gateway g) (db-key (graph-db g) key) value))

(defmethod g-get ((g gateway-graph-view) key)
  (gateway-get (graph-gateway g) (db-key (graph-db g) key)))

(defmethod g-delete ((g gateway-graph-view) key)
  (gateway-delete (graph-gateway g) (db-key (graph-db g) key)))

(defmethod g-scan ((g gateway-graph-view) prefix)
  (loop for (pkey . value) in (gateway-scan-fast (graph-gateway g) (db-key (graph-db g) prefix))
        collect (cons (db-strip (graph-db g) pkey) value)))

(defmethod g-counter ((g gateway-graph-view) key)
  ;; Best-effort read-modify-write through the ring; single-coordinator
  ;; safe.  (Replaced by the atomic +op-counter+ opcode in a later phase.)
  (let ((cur (let ((v (g-get g key))) (if v (read-u64 v 0) 0)))
        (next nil))
    (loop do (setf next (1+ cur))
          (let ((buf (make-buffer)))
            (buf-write-u64 buf next)
            (g-put g key buf))
          (return next))))

;;; ------------------------------------------------------------------
;;; records

(defun %encode-node-record (labels props)
  (codec-encode (cypher-map (list (cons "labels" (cypher-list labels))
                                  (cons "props" (cypher-map props))))))

(defun %encode-rel-record (type start end props)
  (codec-encode (cypher-map (list (cons "type" type)
                                  (cons "start" start)
                                  (cons "end" end)
                                  (cons "props" (cypher-map props))))))

(defun %props-of (rec)
  "Property alist of a decoded record."
  (cypher-map-pairs (%record-get rec "props")))

(defun %funcall (fn &rest args)
  (apply fn args))

(defun %record-get (record key)
  (cdr (assoc key (cypher-map-pairs record) :test #'equal)))

(defun %decode-record (octets)
  "Decode a record; returns its cypher-map."
  (let ((v (codec-decode octets)))
    (unless (cypher-map-p v)
      (error "graph: corrupt record (not a map): ~s" v))
    v))

(defun %blob-ref-p (v)
  (and (cypher-map-p v) (assoc "~blob" (cypher-map-pairs v) :test #'equal)))

(defun %blob-ref-target (v)
  (cdr (assoc "~blob" (cypher-map-pairs v) :test #'equal)))

(defun %resolve-value (g v)
  "Resolve a stored property value (fetching spilled blobs)."
  (if (%blob-ref-p v)
      (g-get g (concatenate 'string "b:" (%blob-ref-target v)))
      v))

(defun %resolve-props (g props)
  (mapcar (lambda (p) (cons (car p) (%resolve-value g (cdr p))))
          (cypher-map-pairs props)))

(defun %spill-prop (g eid k v)
  "Spill large binary values to b:<eid>:<k>; returns the value to store
in the record (the blob reference, or V unchanged)."
  (if (and (typep v '(vector (unsigned-byte 8)))
           (> (length v) +blob-inline-limit+))
      (progn
        (g-put g (format nil "b:~a:~a" eid k) v)
        (cypher-map (list (cons "~blob" (format nil "~a:~a" eid k)))))
      v))

(defun %cleanup-prop (g eid k v)
  "Delete the spilled blob of property (EID . K) with stored value V, if any."
  (when (%blob-ref-p v)
    (g-delete g (concatenate 'string "b:" (%blob-ref-target v)))))

(defun %delete-blobs (g eid)
  (dolist (p (g-scan g (format nil "b:~a:" eid)))
    (g-delete g (car p))))

;;; ------------------------------------------------------------------
;;; derived indexes (local views)

(defun %index-clear (g)
  (clrhash (slot-value g 'node-index))
  (clrhash (slot-value g 'rel-index))
  (clrhash (slot-value g 'label-index))
  (clrhash (slot-value g 'adj-out))
  (clrhash (slot-value g 'adj-in)))

(defun %index-adj (g dir eid type rid)
  (let* ((outer (slot-value g dir))
         (by-type (or (gethash eid outer)
                      (setf (gethash eid outer) (make-hash-table :test #'equal))))
         (by-rid (or (gethash type by-type)
                     (setf (gethash type by-type) (make-hash-table :test #'equal)))))
    (setf (gethash rid by-rid) t)))

(defun %unindex-adj (g dir eid type rid)
  (let* ((outer (slot-value g dir))
         (by-type (gethash eid outer)))
    (when by-type
      (let ((by-rid (gethash type by-type)))
        (when by-rid (remhash rid by-rid))))))

(defun %graph-persist-derived-indexes-p (g)
  "Whether derived label/adjacency index entries should be durable.
Local S3-backed graphs rebuild these indexes from node and relationship
records, avoiding one object per label and two objects per relationship.
Gateway graphs retain the entries because remote expansion uses them."
  (not (and (typep g 'local-graph-view)
            (store-backend (graph-store g)))))

(defun %idx-node-add (g eid)
  (when (typep g 'local-graph-view)
    (setf (gethash eid (slot-value g 'node-index)) t)))

(defun %idx-node-del (g eid)
  (when (typep g 'local-graph-view)
    (remhash eid (slot-value g 'node-index))))

(defun %idx-rel-add (g rid)
  (when (typep g 'local-graph-view)
    (setf (gethash rid (slot-value g 'rel-index)) t)))

(defun %idx-rel-del (g rid)
  (when (typep g 'local-graph-view)
    (remhash rid (slot-value g 'rel-index))))

(defun %idx-label-add (g label eid)
  (when (typep g 'local-graph-view)
    (let ((s (or (gethash label (slot-value g 'label-index))
                 (setf (gethash label (slot-value g 'label-index))
                       (make-hash-table :test #'equal)))))
      (setf (gethash eid s) t))))

(defun %idx-label-del (g label eid)
  (when (typep g 'local-graph-view)
    (let ((s (gethash label (slot-value g 'label-index))))
      (when s (remhash eid s)))))

(defun %idx-adj-add (g dir eid type rid)
  (when (typep g 'local-graph-view)
    (%index-adj g (if (eq dir :out) 'adj-out 'adj-in) eid type rid)))

(defun %idx-adj-del (g dir eid type rid)
  (when (typep g 'local-graph-view)
    (%unindex-adj g (if (eq dir :out) 'adj-out 'adj-in) eid type rid)))

(defun graph-rebuild-indexes (g)
  "Rebuild every derived index of G from the store contents (axiom D2:
indexes are a pure function of the store; a rebuild restores them after
replay or after any inconsistency)."
  (when (typep g 'local-graph-view)
    (%index-clear g)
    (g-map g (lambda (p)
      (let ((k (car p)))
        (cond
          ((and (>= (length k) 2) (string= k "n:" :end1 2))
           (let ((eid (subseq k 2)))
             (setf (gethash eid (slot-value g 'node-index)) t)
             (let* ((rec (%decode-record (cdr p)))
                    (labels (cypher-list-elements (%record-get rec "labels"))))
               (dolist (l labels)
                 (let ((s (or (gethash l (slot-value g 'label-index))
                              (setf (gethash l (slot-value g 'label-index))
                                    (make-hash-table :test #'equal)))))
                   (setf (gethash eid s) t))))))
          ((and (>= (length k) 2) (string= k "r:" :end1 2))
           (let* ((rid (subseq k 2))
                  (rec (%decode-record (cdr p)))
                  (type (%record-get rec "type"))
                  (start (%record-get rec "start"))
                  (end (%record-get rec "end")))
             (setf (gethash rid (slot-value g 'rel-index)) t)
             ;; Adjacency markers are optional for compact S3 stores.  The
             ;; relationship record is authoritative and sufficient to
             ;; reconstruct both directions at startup.
             (%index-adj g 'adj-out start type rid)
             (%index-adj g 'adj-in end type rid)))
          ((and (>= (length k) 3) (string= k "nl:" :end1 3))
           (let* ((rest (subseq k 3))
                  (sep (position #\: rest)))
             (when sep
               (let* ((label (subseq rest 0 sep))
                      (eid (subseq rest (1+ sep)))
                      (s (or (gethash label (slot-value g 'label-index))
                             (setf (gethash label (slot-value g 'label-index))
                                   (make-hash-table :test #'equal)))))
                 (setf (gethash eid s) t)))))
          ((and (>= (length k) 2) (string= k "e:" :end1 2))
           ;; e:<eid>:<o|i>:<type>:<rid>
           (let* ((rest (subseq k 2))
                  (p1 (position #\: rest)))
             (when p1
               (let* ((eid (subseq rest 0 p1))
                      (rest2 (subseq rest (1+ p1)))
                      (dir (if (and (>= (length rest2) 2)
                                    (string= rest2 "o:" :end1 2))
                               :out :in))
                      (rest3 (subseq rest2 2))
                      (p3 (position #\: rest3)))
                 (when p3
                   (let ((type (subseq rest3 0 p3))
                         (rid (subseq rest3 (1+ p3))))
                     (%index-adj g (if (eq dir :out) 'adj-out 'adj-in) eid type rid)))))))))))
    g))

;;; ------------------------------------------------------------------
;;; id minting

(defun graph-mint-id (g)
  "Mint a fresh element id (globally unique per database)."
  (format nil "g~16,'0x" (g-counter g "g:next")))

;;; ------------------------------------------------------------------
;;; element operations

(defun graph-create-node (g &key labels props)
  "Create a node with LABELS (list of strings) and PROPS (alist of
(string . cypher-value)).  Returns the new element id."
  (let ((eid (graph-mint-id g)))
    (let ((spilled (mapcar (lambda (p) (cons (car p) (%spill-prop g eid (car p) (cdr p))))
                           props)))
      (g-put g (format nil "n:~a" eid) (%encode-node-record labels spilled))
      (%idx-node-add g eid)
      (dolist (l labels)
        (when (%graph-persist-derived-indexes-p g)
          (g-put g (format nil "nl:~a:~a" l eid) #()))
        (%idx-label-add g l eid))
      eid)))

(defun graph-create-relationship (g type start end &key props)
  "Create a relationship of TYPE from START to END (element ids).
Axiom A1: both endpoints must exist; TYPE is the single type label.
Returns the new relationship id."
  (unless (and (g-get g (format nil "n:~a" start))
               (g-get g (format nil "n:~a" end)))
    (error "graph: relationship endpoints must exist (start ~a, end ~a)"
           start end))
  (let ((rid (graph-mint-id g)))
    (let ((spilled (mapcar (lambda (p) (cons (car p) (%spill-prop g rid (car p) (cdr p))))
                           props)))
      (g-put g (format nil "r:~a" rid) (%encode-rel-record type start end spilled))
      (when (%graph-persist-derived-indexes-p g)
        (g-put g (format nil "e:~a:o:~a:~a" start type rid) #())
        (g-put g (format nil "e:~a:i:~a:~a" end type rid) #()))
      (%idx-rel-add g rid)
      (%idx-adj-add g :out start type rid)
      (%idx-adj-add g :in end type rid)
      rid)))

(defun graph-node (g eid)
  "Return the node as (:id <eid> :labels (..) :props alist), or NIL."
  (let ((v (g-get g (format nil "n:~a" eid))))
    (when v
      (let ((rec (%decode-record v)))
        (list :id eid
              :labels (cypher-list-elements (%record-get rec "labels"))
              :props (%resolve-props g (%record-get rec "props")))))))

(defun graph-relationship (g rid)
  "Return the relationship as (:id <rid> :type t :start s :end e
:props alist), or NIL."
  (let ((v (g-get g (format nil "r:~a" rid))))
    (when v
      (let ((rec (%decode-record v)))
        (list :id rid
              :type (%record-get rec "type")
              :start (%record-get rec "start")
              :end (%record-get rec "end")
              :props (%resolve-props g (%record-get rec "props")))))))

(defun graph-node-property (g eid k)
  "Read property K of node EID (a Cypher value, or :cypher-null)."
  (let ((node (graph-node g eid)))
    (let ((p (assoc k (getf node :props) :test #'equal)))
      (if p (cdr p) :cypher-null))))

(defun graph-relationship-property (g rid k)
  (let ((rel (graph-relationship g rid)))
    (let ((p (assoc k (getf rel :props) :test #'equal)))
      (if p (cdr p) :cypher-null))))

(defun %update-node-record (g eid fn)
  "Read node EID, call FN on (labels . props) and write it back.
FN receives the decoded record alist and returns a new one."
  (let ((v (g-get g (format nil "n:~a" eid))))
    (when v
      (let* ((rec (%decode-record v))
             (props (%props-of rec))
             (new (%funcall fn rec (cypher-list-elements (%record-get rec "labels")) props)))
        (g-put g (format nil "n:~a" eid) (%encode-node-record
                                          (cypher-list-elements (%record-get new "labels"))
                                          (%record-get new "props")))
        eid))))

(defun graph-set-node-property (g eid k v)
  "Set property K of node EID to Cypher value V.  Binary values larger
than +blob-inline-limit+ are spilled to their own key."
  (%update-node-record
   g eid
   (lambda (rec labels props)
     (let ((old (cdr (assoc k props :test #'equal))))
       (%cleanup-prop g eid k old)
       (let ((props2 (remove k props :key #'car :test #'equal)))
         (list (cons "labels" (cypher-list labels))
               (cons "props" (acons k (%spill-prop g eid k v) props2)))))))
  v)

(defun graph-remove-node-property (g eid k)
  (%update-node-record
   g eid
   (lambda (rec labels props)
     (let ((old (cdr (assoc k props :test #'equal))))
       (%cleanup-prop g eid k old)
       (list (cons "labels" (cypher-list labels))
             (cons "props" (remove k props :key #'car :test #'equal))))))
  t)

(defun %update-rel-record (g rid fn)
  (let ((v (g-get g (format nil "r:~a" rid))))
    (when v
      (let* ((rec (%decode-record v))
             (new (%funcall fn rec)))
        (g-put g (format nil "r:~a" rid)
               (%encode-rel-record (%record-get new "type")
                                   (%record-get new "start")
                                   (%record-get new "end")
                                   (%record-get new "props")))
        rid))))

(defun graph-set-relationship-property (g rid k v)
  (%update-rel-record
   g rid
   (lambda (rec)
     (let ((props (%props-of rec)))
       (let ((old (cdr (assoc k props :test #'equal))))
         (%cleanup-prop g rid k old)
         (list (cons "type" (%record-get rec "type"))
               (cons "start" (%record-get rec "start"))
               (cons "end" (%record-get rec "end"))
               (cons "props" (acons k (%spill-prop g rid k v)
                                    (remove k props :key #'car :test #'equal))))))))
  v)

(defun graph-remove-relationship-property (g rid k)
  (%update-rel-record
   g rid
   (lambda (rec)
     (let ((props (%props-of rec)))
       (let ((old (cdr (assoc k props :test #'equal))))
         (%cleanup-prop g rid k old)
         (list (cons "type" (%record-get rec "type"))
               (cons "start" (%record-get rec "start"))
               (cons "end" (%record-get rec "end"))
               (cons "props" (remove k props :key #'car :test #'equal)))))))
  t)

(defun graph-add-node-label (g eid label)
  (let ((node (graph-node g eid)))
    (unless node (error "graph: no such node ~a" eid))
    (unless (member label (getf node :labels) :test #'equal)
      (g-put g (format nil "nl:~a:~a" label eid) #())
      (%idx-label-add g label eid)
      (%update-node-record
       g eid
       (lambda (rec labels props)
         (list (cons "labels" (cypher-list (cons label labels)))
               (cons "props" props))))))
  eid)

(defun graph-remove-node-label (g eid label)
  (let ((node (graph-node g eid)))
    (when (and node (member label (getf node :labels) :test #'equal))
      (g-delete g (format nil "nl:~a:~a" label eid))
      (%idx-label-del g label eid)
      (%update-node-record
       g eid
       (lambda (rec labels props)
         (list (cons "labels" (cypher-list (remove label labels :test #'equal)))
               (cons "props" props))))))
  eid)

;;; ------------------------------------------------------------------
;;; reads: scans and expansion

(defun %ensure-node-index (g)
  (when (and (typep g 'local-graph-view)
             (zerop (hash-table-count (slot-value g 'node-index))))
    (let* ((store (graph-store g)) (backend (store-backend (graph-store g)))
           (inner (when (typep backend 'encrypted-storage-plugin)
                    (encrypted-storage-plugin-inner backend)))
           (lazy-s3 (cond ((typep backend 's3-config) (s3-config-lazy backend))
                          ((and inner (typep inner 's3-config)) (s3-config-lazy inner))
                          (t nil)))
           (index (storage-plugin-lazy-index backend)))
      (if (and lazy-s3 index (plusp (hash-table-count index)))
          (maphash
           (lambda (key entry)
             (declare (ignore entry))
             (when (and (>= (length key) 2) (string= key "n:" :end1 2))
               (let* ((eid (subseq key 2)) (rec (%decode-record (store-get store key)))
                      (labels (cypher-list-elements (%record-get rec "labels"))))
                 (setf (gethash eid (slot-value g 'node-index)) t)
                 (dolist (label labels)
                   (let ((bucket (or (gethash label (slot-value g 'label-index))
                                     (setf (gethash label (slot-value g 'label-index))
                                           (make-hash-table :test #'equal)))))
                     (setf (gethash eid bucket) t))))))
           index)
          (g-map g
                 (lambda (p)
                   (when (and (>= (length (car p)) 2)
                              (string= (car p) "n:" :end1 2))
                     (let* ((eid (subseq (car p) 2))
                            (rec (%decode-record (cdr p)))
                            (labels (cypher-list-elements (%record-get rec "labels"))))
                       (setf (gethash eid (slot-value g 'node-index)) t)
                       (dolist (label labels)
                         (let ((bucket (or (gethash label (slot-value g 'label-index))
                                           (setf (gethash label (slot-value g 'label-index))
                                                 (make-hash-table :test #'equal)))))
                           (setf (gethash eid bucket) t)))))))))))

(defun %ensure-rel-index (g)
  (when (and (typep g 'local-graph-view)
             (zerop (hash-table-count (slot-value g 'rel-index))))
    (let ((backend (store-backend (graph-store g))))
      (if (and (typep backend 's3-config) (s3-config-lazy backend))
          (maphash (lambda (key entry)
                     (declare (ignore entry))
                     (when (and (>= (length key) 2) (string= key "r:" :end1 2))
                       (setf (gethash (subseq key 2) (slot-value g 'rel-index)) t)))
                   (s3-config-lazy-index backend))
          (g-map g (lambda (p)
                     (when (and (>= (length (car p)) 2)
                                (string= (car p) "r:" :end1 2))
                       (setf (gethash (subseq (car p) 2) (slot-value g 'rel-index)) t))))))))

(defun %gateway-summary-node-ids (g label)
  (let ((gw (graph-gateway g)) (ids (make-hash-table :test #'equal)) (healthy 0)
        (prefix (db-key (graph-db g) "")))
    (dolist (peer (gateway-peers gw))
      (let ((reply (ignore-errors
                     (gateway-request gw (car peer)
                                      (list :op #.+op-label-ids+ :prefix prefix :label label)))))
        (when (and reply (eql (getf reply :status) #.+status-ok+))
          (incf healthy)
          (dolist (id (cypher-list-elements
                       (car (multiple-value-list (codec-decode (getf reply :value))))))
            (setf (gethash id ids) t)))))
    (when (plusp healthy)
      (cypher-list (sort (loop for id being the hash-keys of ids collect id) #'string<)))))

(defun graph-scan-node-ids (g &key label)
  "Element ids of all nodes (with LABEL when given), sorted."
  (if (typep g 'gateway-graph-view)
      (if label
          (or (%gateway-summary-node-ids g label)
              (let ((prefix (format nil "nl:~a:" label)) (out nil))
                (dolist (p (g-scan g prefix))
                  (push (subseq (car p) (1+ (position #\: (car p) :from-end t))) out))
                (sort (remove-duplicates out :test #'equal) #'string<)))
          (let ((out nil))
            (dolist (p (g-scan g "n:")) (push (subseq (car p) 2) out))
            (sort (remove-duplicates out :test #'equal) #'string<)))
      (if (typep g 'local-graph-view)
      (progn (%ensure-node-index g)
        (let ((out nil))
        (if label
            (let ((s (gethash label (slot-value g 'label-index))))
              (when s
                (maphash (lambda (eid v) (declare (ignore v)) (push eid out)) s)))
            (maphash (lambda (eid v) (declare (ignore v)) (push eid out))
                     (slot-value g 'node-index)))
        (sort out #'string<)))
      (let ((prefix (if label (format nil "nl:~a:" label) "n:"))
            (out nil))
        (dolist (p (g-scan g prefix))
          (let ((k (car p)))
            (push (if label
                      (subseq k (1+ (position #\: k :from-end t)))
                      (subseq k 2))
                  out)))
        (sort (remove-duplicates out :test #'equal) #'string<))))
)
(defun graph-scan-rel-ids (g &key type)
  "Element ids of all relationships (of TYPE when given), sorted."
  (if (typep g 'local-graph-view)
      (progn (%ensure-rel-index g)
        (let ((out nil))
        (maphash (lambda (rid v) (declare (ignore v))
                   (when (or (null type)
                             (equal type (getf (graph-relationship g rid) :type)))
                     (push rid out)))
                 (slot-value g 'rel-index))
        (sort out #'string<)))
      (let ((out nil))
        (dolist (p (g-scan g "r:"))
          (let* ((rid (subseq (car p) 2))
                 (rec (%decode-record (cdr p))))
            (when (or (null type) (equal type (%record-get rec "type")))
              (push rid out))))
        (sort out #'string<))))

(defun %expand-rids (g eid dir)
  "Relationship ids incident to EID in direction DIR (:out, :in, or :both)."
  (if (typep g 'local-graph-view)
      (let ((seen (make-hash-table :test #'equal)) (out nil))
        (dolist (d (if (eq dir :both) '(:out :in) (list dir)))
          (let ((by-type (gethash eid (slot-value g (if (eq d :out) 'adj-out 'adj-in)))))
            (when by-type
              (maphash (lambda (type by-rid)
                         (declare (ignore type))
                         (maphash (lambda (rid v)
                                    (declare (ignore v))
                                    (unless (gethash rid seen)
                                      (setf (gethash rid seen) t)
                                      (push rid out)))
                                  by-rid))
                       by-type))))
        out)
      (let ((out nil))
        (dolist (d (if (eq dir :both) '("o" "i") (list (if (eq dir :out) "o" "i"))))
          (dolist (p (g-scan g (format nil "e:~a:~a:" eid d)))
            ;; e:<eid>:<dir>:<type>:<rid>
            (let* ((k (car p))
                   (tail (subseq k (length (format nil "e:~a:~a:" eid d))))
                   (sep (position #\: tail)))
              (when sep
                (push (subseq tail (1+ sep)) out)))))
        (remove-duplicates out :test #'equal))))

(defun graph-expand (g eid &key (dir :out) type)
  "Pairs ((rid . neighbor-eid) ...) of relationships incident to EID in
direction DIR (:out, :in, :both), optionally filtered by TYPE, sorted
by RID."
  (let ((out nil))
    (dolist (rid (%expand-rids g eid dir))
      (let ((rel (graph-relationship g rid)))
        (when rel
          (let ((rtype (getf rel :type))
                (neighbor (if (equal eid (getf rel :end))
                              (getf rel :start)
                              (getf rel :end))))
            (when (or (null type) (equal type rtype))
              (push (cons rid neighbor) out))))))
    (sort out #'string< :key #'car)))

;;; ------------------------------------------------------------------
;;; deletions

(defun graph-delete-relationship (g rid)
  "Delete relationship RID (and its spilled blobs and adjacency entries)."
  (let ((rel (graph-relationship g rid)))
    (when rel
      (let ((type (getf rel :type))
            (start (getf rel :start))
            (end (getf rel :end)))
        (when (%graph-persist-derived-indexes-p g)
          (g-delete g (format nil "e:~a:o:~a:~a" start type rid))
          (g-delete g (format nil "e:~a:i:~a:~a" end type rid)))
        (%idx-adj-del g :out start type rid)
        (%idx-adj-del g :in end type rid)
        (%delete-blobs g rid)
        (g-delete g (format nil "r:~a" rid))
        (%idx-rel-del g rid)
        t))))

(defun graph-delete-node (g eid &key (detach nil))
  "Delete node EID.  Without :detach, fails when relationships are
incident (axiom A1 forbids dangling endpoints)."
  (let ((incident (%expand-rids g eid :both)))
    (when (and incident (not detach))
      (error "graph: node ~a still has ~d relationship~:p; use :detach t" eid (length incident)))
    (dolist (rid incident)
      (graph-delete-relationship g rid))
    (let ((node (graph-node g eid)))
      (when node
        (dolist (l (getf node :labels))
          (when (%graph-persist-derived-indexes-p g)
            (g-delete g (format nil "nl:~a:~a" l eid)))
          (%idx-label-del g l eid))
        (%delete-blobs g eid)
        (g-delete g (format nil "n:~a" eid))
        (%idx-node-del g eid)
        t))))

;;; ------------------------------------------------------------------
;;; counts and invariants

(defun graph-store-counts (store &key (db +default-db+))
    "Count authoritative records without materializing scan results."
  (let ((nodes 0) (rels 0) (labels (make-hash-table :test #'equal))
        (prefix (db-key db ""))
        (backend (store-backend store)))
    (if (and (typep backend 's3-config) (s3-config-lazy backend))
        (let ((counts (gethash db (s3-config-lazy-counts backend))))
          (if counts
              (progn
                (setf nodes (car counts) rels (cdr counts))
                (let ((known (gethash db (s3-config-lazy-labels backend))))
                  (when known
                    (maphash (lambda (label count) (setf (gethash label labels) count)) known))))
              (maphash
               (lambda (key entry)
                 (declare (ignore entry))
                 (when (and (>= (length key) (length prefix))
                            (string= prefix key :end2 (length prefix)))
                   (let ((local (db-strip db key)))
                     (cond
                       ((and (>= (length local) 2) (string= local "n:" :end1 2))
                        (incf nodes))
                       ((and (>= (length local) 2) (string= local "r:" :end1 2))
                        (incf rels))))))
               (s3-config-lazy-index backend))))
        (store-map
         store
         (lambda (key value)
           (when (and (>= (length key) (length prefix))
                      (string= prefix key :end2 (length prefix)))
             (let ((local (db-strip db key)))
               (cond
                 ((and (>= (length local) 2) (string= local "n:" :end1 2))
                  (incf nodes)
                  (dolist (label (cypher-list-elements
                                  (%record-get (%decode-record value) "labels")))
                    (setf (gethash label labels) t)))
                 ((and (>= (length local) 2) (string= local "r:" :end1 2))
                  (incf rels))))))))
    (values nodes rels labels)))

(defun %count-store-keys (g prefix)
  "Count keys starting with PREFIX by walking live storage exactly once."
  (let ((n 0))
    (g-map g
           (lambda (p)
             (when (and (>= (length (car p)) 2)
                        (string= (car p) prefix :end1 2))
               (incf n))))
    n))

(defun graph-count-nodes (g)
  (when (typep g 'local-graph-view) (%ensure-node-index g))
  (if (typep g 'local-graph-view)
      (let ((n (hash-table-count (slot-value g 'node-index))))
        (if (plusp n)
            n
            (%count-store-keys g "n:")))

      (length (graph-scan-node-ids g))))

(defun graph-count-rels (g)
  (when (typep g 'local-graph-view) (%ensure-node-index g))
  (if (typep g 'local-graph-view)
      (let ((n (hash-table-count (slot-value g 'rel-index))))
        (if (plusp n)
            n
            (%count-store-keys g "r:")))

      (length (graph-scan-rel-ids g))))

(defun graph-stream-relationships (g fn &key type limit)
  "Call FN for each primary-owned relationship without materializing ids.
When LIMIT is supplied, stop traversal after that many matching records."
  (let ((seen 0))
    (catch 'graph-stream-stop
      (g-map g
             (lambda (p)
               (let ((key (car p)))
                 (when (and (>= (length key) 2) (string= key "r:" :end1 2))
                   (let* ((rid (subseq key 2)) (rec (%decode-record (cdr p)))
                          (rtype (%record-get rec "type")))
                     (when (or (null type) (equal type rtype))
                       (incf seen)
                       (funcall fn
                                (list :id rid :type rtype
                                      :start (%record-get rec "start")
                                      :end (%record-get rec "end")
                                      :props (%resolve-props g
                                                              (%record-get rec "props"))))
                       (when (and limit (>= seen limit))
                         (throw 'graph-stream-stop nil)))))))))))

(defun graph-check-invariants (g)
  "Verify axioms A1-A3 and index consistency."
  (let ((violations nil))
    (dolist (p (g-scan g "r:"))
      (let* ((rid (subseq (car p) 2))
             (rec (%decode-record (cdr p)))
             (type (%record-get rec "type"))
             (start ( %record-get rec "start"))
             (end (%record-get rec "end")))
        (unless (and (stringp type) (plusp (length type)))
          (push (format nil "rel ~a has no type" rid) violations))
        (unless (g-get g (format nil "n:~a" start))
          (push (format nil "rel ~a start ~a missing" rid start) violations))
        (unless (g-get g (format nil "n:~a" end))
          (push (format nil "rel ~a end ~a missing" rid end) violations))
        (when (%graph-persist-derived-indexes-p g)
          (unless (g-get g (format nil "e:~a:o:~a:~a" start type rid))
            (push (format nil "rel ~a missing out-adjacency entry" rid) violations))
          (unless (g-get g (format nil "e:~a:i:~a:~a" end type rid))
            (push (format nil "rel ~a missing in-adjacency entry" rid) violations)))))
    (dolist (p (g-scan g "nl:"))
      (let* ((k (car p)) (rest (subseq k 3)) (sep (position #\: rest)))
        (when sep
          (let ((label (subseq rest 0 sep)) (eid (subseq rest (1+ sep))))
            (unless (g-get g (format nil "n:~a" eid))
              (push (format nil "label entry ~a -> missing node ~a" label eid) violations))))))
    (dolist (p (g-scan g "e:"))
      (let* ((k (car p)) (rest (subseq k 2)) (p1 (position #\: rest)))
        (when p1
          (let* ((eid (subseq rest 0 p1))
                 (dir (and (> (length rest) (+ p1 1))
                           (char= (char rest (1+ p1)) #\o)))
                 (rest3 (subseq rest (+ p1 3)))
                 (p3 (position #\: rest3)))
            (when p3
              (let* ((type (subseq rest3 0 p3)) (rid (subseq rest3 (1+ p3)))
                     (rel (graph-relationship g rid)))
                (unless rel
                  (push (format nil "adjacency entry ~a -> missing rel" k) violations))
                (when (and rel
                           (not (and (equal (getf rel :type) type)
                                     (equal (if dir (getf rel :start) (getf rel :end)) eid))))
                  (push (format nil "adjacency entry ~a inconsistent" k) violations))))))))
    violations))
