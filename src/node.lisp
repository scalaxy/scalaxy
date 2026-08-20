;;;; node.lisp --- a single Scalaxy storage node

(in-package #:scalaxy)

(defstruct (node (:constructor %make-node (id store replicator followers started-at)))
  id
  store
  replicator
  followers   ; list of (follower-id . transport-fn)
  started-at  ; universal time when the node was created
  (graphs (make-hash-table :test #'equal))) ; db name -> cached graph-view


(defvar *node-outboxes* (make-hash-table :test #'eq))
(defvar *node-quorums* (make-hash-table :test #'eq))
(defvar *node-last-replication-errors* (make-hash-table :test #'eq))
(defun node-outbox (node) (gethash node *node-outboxes*))
(defun (setf node-outbox) (value node) (setf (gethash node *node-outboxes*) value))
(defun node-quorum (node) (gethash node *node-quorums* 0))
(defun (setf node-quorum) (value node) (setf (gethash node *node-quorums*) value))
(defun node-last-replication-error (node) (gethash node *node-last-replication-errors*))
(defun (setf node-last-replication-error) (value node) (setf (gethash node *node-last-replication-errors*) value))
(defun node-retry-replication (node)
  (let ((acked 0) (remaining nil))
    (dolist (entry (node-outbox node))
      (let ((f (assoc (car entry) (node-followers node) :test #'equal)))
        (let ((reply (and f (ignore-errors (funcall (cdr f) (cdr entry))))))
          (if (and reply (eql (getf reply :status) #.+status-ok+))
              (incf acked) (push entry remaining)))))
    (setf (node-outbox node) remaining) acked))

(defvar *node-counter* 0)

(defun make-node (&key (id nil) (store (make-store)) (quorum 0))
  (incf *node-counter*)
  (let ((node (%make-node (or id (format nil "node-~d" *node-counter*))
                          store (make-replicator) nil (get-universal-time))))
    (let ((v (ignore-errors (store-get store "__scalaxy_replication_seq__"))))
      (when (and v (vectorp v) (>= (length v) 8))
        (setf (replicator-seq (node-replicator node)) (read-u64 v 0))))
    (setf (node-quorum node) quorum) node))

(defun node-next-seq (node)
  (let ((seq (replicator-seq (node-replicator node))))
    (setf (replicator-seq (node-replicator node)) (1+ seq))
    (ignore-errors
      (let ((b (make-buffer)))
        (buf-write-u64 b (replicator-seq (node-replicator node)))
        (store-put (node-store node) "__scalaxy_replication_seq__" b)))
    seq))

(defun node-add-follower (node follower-id transport)
  (push (cons follower-id transport) (node-followers node))
  node)

(defun node-replicate (node key value)
  "Replicate synchronously, retaining failed follower messages for retry."
  (let ((acked 0) (seq (node-next-seq node)))
    (replicator-record (node-replicator node) seq (list :key key :value value))
    (dolist (f (node-followers node))
      (let ((msg (list :op #.+op-replicate+ :seq seq :sub-op #.+op-put+ :key key :value value)))
        (let ((reply (ignore-errors (funcall (cdr f) msg))))
          (if (and reply (eql (getf reply :status) #.+status-ok+))
              (incf acked)
              (progn (push (cons (car f) msg) (node-outbox node))
                     (setf (node-last-replication-error node) (car f)))))))
    (when (and (plusp (node-quorum node)) (< acked (node-quorum node)))
      (error "replication quorum unavailable: ~d/~d acknowledgements" acked (node-quorum node)))
    acked))

(defun node-put (node key value)
  (store-put (node-store node) key value)
  (when (fboundp 'invalidate-node-graph-metrics) (invalidate-node-graph-metrics node))
  (node-replicate node key value)
  value)

(defun node-delete (node key)
  (let ((present? (store-delete (node-store node) key)))
    (when present?
      (when (fboundp 'invalidate-node-graph-metrics) (invalidate-node-graph-metrics node))
      (let ((acked 0) (seq (node-next-seq node)))
        (dolist (f (node-followers node))
          (let ((msg (list :op #.+op-replicate+ :seq seq :sub-op #.+op-delete+ :key key)))
            (let ((reply (ignore-errors (funcall (cdr f) msg))))
              (if (and reply (eql (getf reply :status) #.+status-ok+))
                  (incf acked)
                  (progn (push (cons (car f) msg) (node-outbox node))
                         (setf (node-last-replication-error node) (car f)))))))
        (when (and (plusp (node-quorum node)) (< acked (node-quorum node)))
          (error "replication quorum unavailable: ~d/~d acknowledgements" acked (node-quorum node)))))
    present?))

(defun node-get (node key)
  (store-get (node-store node) key))

(defun node-scan (node prefix)
  (store-scan (node-store node) prefix))

(defun %node-aggregate-id-set (encoded)
  (let ((set (make-hash-table :test #'equal)))
    (dolist (id (remove "" (uiop:split-string (or encoded "") :separator ",") :test #'equal))
      (setf (gethash id set) t))
    (and (plusp (hash-table-count set)) set)))

(defun %node-lazy-label-map (store prefix)
  (let ((labels (make-hash-table :test #'equal)) (cfg (store-backend store)))
    (maphash
     (lambda (key entry)
       (declare (ignore entry))
       (when (and (>= (length key) (length prefix))
                  (string= prefix key :end2 (length prefix)))
         (let ((local (subseq key (length prefix))))
           (when (and (>= (length local) 2) (string= local "n:" :end1 2))
             (let* ((value (%s3-lazy-get cfg key))
                    (rec (and value (%decode-record value)))
                    (ls (and rec (%record-get rec "labels"))))
               (when rec
                 (setf (gethash (subseq local 2) labels)
                       (if (cypher-list-p ls) (cypher-list-elements ls) nil))))))))
     (s3-config-lazy-index cfg))
    labels))

(defun %node-endpoint-aggregate (node msg)
  "Aggregate relationship endpoint pairs without reading packed payloads."
  (let* ((store (node-store node)) (cfg (store-backend store))
         (prefix (getf msg :prefix))
         (sep (and prefix (position #\: prefix :start 2)))
         (left (%node-aggregate-id-set (getf msg :left-ids)))
         (right (%node-aggregate-id-set (getf msg :right-ids)))
         (table (and cfg (s3-config-lazy-endpoint-aggregates cfg)))
         (count 0) (sum 0) (sum-seen nil))
    (when (and sep left right table (not (gethash :disabled table)))
      (let ((db (subseq prefix 2 sep)) (type (getf msg :type))
            (property (getf msg :property)))
        (maphash
         (lambda (key values)
           (when (and (listp key) (equal (first key) db)
                      (equal (second key) type)
                      (gethash (third key) left)
                      (gethash (fourth key) right))
             (incf count (gethash "~count" values 0))
             (when (plusp (length property))
               (multiple-value-bind (value present) (gethash property values)
                 (when present (incf sum value) (setf sum-seen t))))))
         table)
        (values (if (string-equal (getf msg :function) "COUNT") count
                    (if sum-seen sum :cypher-null)) t)))))

(defun %node-lazy-filtered-aggregate (node msg)
  "Aggregate packed relationships using globally supplied endpoint IDs."
  (let* ((store (node-store node)) (cfg (store-backend store))
         (prefix (getf msg :prefix)) (type (getf msg :type))
         (property (getf msg :property))
         (left-ids (%node-aggregate-id-set (getf msg :left-ids)))
         (right-ids (%node-aggregate-id-set (getf msg :right-ids)))
         (labels (%node-lazy-label-map store prefix))
         (count 0) (sum 0) (sum-seen nil)
         (cache (s3-config-lazy-aggregate-cache cfg))
         (cache-key (list prefix type (getf msg :function) property
                          (getf msg :left-label) (getf msg :right-label)))
         (cached-pair (multiple-value-list (gethash cache-key cache))))
    (when (second cached-pair)
      (return-from %node-lazy-filtered-aggregate (first cached-pair)))
    (multiple-value-bind (fast-value fast-p)
        (%node-endpoint-aggregate node msg)
      (when fast-p
        (setf (gethash cache-key cache) fast-value)
        (%s3-aggregate-cache-save cfg)
        (return-from %node-lazy-filtered-aggregate fast-value)))
    (labels ((has-id (id ids wanted)
               (or (and ids (gethash id ids))
                   (and (null ids) (plusp (length wanted))
                        (member wanted (or (gethash id labels)
                                           (gethash (princ-to-string id) labels))
                                :test #'string-equal)))))
      (maphash
       (lambda (relative ignored)
         (declare (ignore ignored))
         (let ((bytes (%s3-get-cached cfg relative)))
           (multiple-value-bind (outer pos) (read-u32 bytes 1)
             (declare (ignore outer))
             (multiple-value-bind (magic next) (%codec-read bytes pos)
               (declare (ignore magic))
               (when (= (aref bytes next) +tag-list+)
                 (multiple-value-bind (n cursor0) (read-u32 bytes (1+ next))
                   (let ((cursor cursor0))
                     (loop repeat n
                           do (when (= (aref bytes cursor) +tag-list+)
                                (multiple-value-bind (fields p0) (read-u32 bytes (1+ cursor))
                                  (declare (ignore fields))
                                  (multiple-value-bind (op p1) (%codec-read bytes p0)
                                    (multiple-value-bind (key p2) (%codec-read bytes p1)
                                      (let ((after (%codec-skip bytes p2)))
                                        (when (and (string= op "PUT")
                                                   (>= (length key) (length prefix))
                                                   (string= prefix key :end2 (length prefix))
                                                   (string= (subseq key (length prefix)) "r:" :end1 2))
                                          (let ((entry (gethash key (s3-config-lazy-index cfg))))
                                            (when (and entry (equal relative (first entry))
                                                       (= p2 (second entry)))
                                              (let* ((raw (car (multiple-value-list (%codec-read bytes p2))))
                                                     (rec (%decode-record raw))
                                                     (rtype (%record-get rec "type"))
                                                     (start (princ-to-string (%record-get rec "start")))
                                                     (end (princ-to-string (%record-get rec "end"))))
                                                (when (and (or (zerop (length type)) (equal type rtype))
                                                           (has-id start left-ids (getf msg :left-label))
                                                           (has-id end right-ids (getf msg :right-label)))
                                                  (incf count)
                                                  (when (plusp (length property))
                                                    (let ((v (cdr (assoc property (%props-of rec) :test #'equal))))
                                                      (unless (or (null v) (eq v :cypher-null))
                                                        (incf sum v) (setf sum-seen t)))))))))
                                        (setf cursor after))))))))))))))
       (s3-config-lazy-segments cfg)))
    (let ((result (if (string-equal (getf msg :function) "COUNT") count
                      (if sum-seen sum :cypher-null))))
      (setf (gethash cache-key cache) result)
      (%s3-aggregate-cache-save cfg)
      result)))

(defun node-aggregate-relationships (node msg)
  "Compute a scalar relationship aggregate locally, without returning rows."
  (let ((prefix (getf msg :prefix)) (type (getf msg :type))
        (property (getf msg :property)) (count 0) (sum 0) (sum-seen nil)
        (store (node-store node)))
    (when (and (or (plusp (length (or (getf msg :left-label) "")))
                   (plusp (length (or (getf msg :right-label) ""))))
               (store-backend store)
               (typep (store-backend store) 's3-config)
               (s3-config-lazy (store-backend store)))
      (return-from node-aggregate-relationships
        (%node-lazy-filtered-aggregate node msg)))
    (when (and (string-equal (getf msg :function) "SUM")
               (plusp (length (or (getf msg :property) "")))
               (store-backend store)
               (typep (store-backend store) 's3-config)
               (s3-config-lazy (store-backend store))
               (s3-config-summary-valid (store-backend store)))
      (let ((sep (position #\: prefix :start 2)))
        (when sep
          (let* ((db (subseq prefix 2 sep))
                 (sums (gethash db (s3-config-lazy-sums (store-backend store))))
                 (key (list type (getf msg :property))))
            (when sums
              (if (plusp (length type))
                  (return-from node-aggregate-relationships (gethash key sums 0))
                  (return-from node-aggregate-relationships
                    (loop for v being the hash-values of sums sum v))))))))
    (when (and (string-equal (getf msg :function) "COUNT")
               (zerop (length (or (getf msg :property) "")))
               (store-backend store)
               (typep (store-backend store) 's3-config)
               (s3-config-lazy (store-backend store))
               (s3-config-summary-valid (store-backend store)))
      (let ((sep (position #\: prefix :start 2)))
        (when sep
          (let* ((db (subseq prefix 2 sep))
                 (types (gethash db (s3-config-lazy-type-counts (store-backend store)))))
            (when types
              (return-from node-aggregate-relationships
                (if (plusp (length type)) (gethash type types 0)
                    (loop for v being the hash-values of types sum v))))))))
    (labels ((visit (key value)
               (when (and (>= (length key) (length prefix))
                          (string= prefix key :end2 (length prefix)))
                 (let ((local (subseq key (length prefix))))
                   (when (and (>= (length local) 2) (string= local "r:" :end1 2))
                     (let* ((rtype (if (plusp (length property))
                                       (%codec-map-field-light value "type")
                                       (%codec-map-field-light value "type"))))
                       (when (or (null type) (zerop (length type)) (equal type rtype))
                         (incf count)
                         (when (plusp (length property))
                           (let* ((rec (%decode-record value))
                                  (v (cdr (assoc property (%props-of rec) :test #'equal))))
                             (unless (or (null v) (eq v :cypher-null))
                               (incf sum v) (setf sum-seen t)))))))))))
      (if (and (store-backend store) (typep (store-backend store) 's3-config)
               (s3-config-lazy (store-backend store)))
          (maphash (lambda (key entry)
                     (declare (ignore entry))
                     (visit key (store-get store key)))
                   (s3-config-lazy-index (store-backend store)))
          (store-map store (lambda (key value) (visit key value))))
    (if (string-equal (getf msg :function) "COUNT") count
        (if sum-seen sum :cypher-null))))

)

(defun %node-topk-summary (node msg)
  (let* ((store (node-store node)) (cfg (store-backend store))
         (prefix (getf msg :prefix)) (sep (position #\: prefix :start 2)))
    (when (and cfg (typep cfg 's3-config) (s3-config-lazy cfg)
               (s3-config-summary-valid cfg) sep)
      (let* ((db (subseq prefix 2 sep))
             (index (gethash db (s3-config-lazy-topk-summaries cfg)))
             (key (list (getf msg :type) (getf msg :property)
                        (if (getf msg :descending) :desc :asc)))
             (values (and index (gethash key index))))
        (when values
          (cypher-list
           (mapcar (lambda (p) (cypher-list (list (cdr p) (car p))))
                   (subseq values 0 (min (getf msg :limit) (length values))))))))))

(defun %node-label-ids-summary (node msg)
  (let* ((store (node-store node)) (cfg (store-backend store))
         (prefix (getf msg :prefix)) (sep (position #\: prefix :start 2)))
    (when (and cfg (typep cfg 's3-config) (s3-config-lazy cfg)
               (s3-config-summary-valid cfg) sep)
      (let* ((db (subseq prefix 2 sep))
             (by-label (gethash db (s3-config-lazy-label-ids cfg)))
             (ids (and by-label (gethash (getf msg :label) by-label))))
        (when ids
          (cypher-list (sort (loop for id being the hash-keys of ids collect id)
                             #'string<)))))))

(defun node-dispatch (node msg)
  "Handle a decoded request message and return a reply message plist.
Writes go through NODE-PUT/NODE-DELETE so they are replicated to the
node's followers (in-process or TCP) before the client is acknowledged."
  (case (getf msg :op)
    (#.+op-bulk-put+
     (let* ((pairs (getf msg :pairs)) (count (length pairs)) (acked 0))
       ;; Write the owner batch once, then send one packed replication frame
       ;; per follower.  This avoids one S3 PUT and one TCP round trip per key.
       (with-s3-batch ()
         (dolist (pair pairs)
           (store-put (node-store node) (car pair) (cdr pair)))
         (when (fboundp 'invalidate-node-graph-metrics)
           (invalidate-node-graph-metrics node))
         (when (plusp count)
           (dolist (f (node-followers node))
             (let ((reply (ignore-errors
                            (funcall (cdr f)
                                     (list :op #.+op-bulk-replicate+
                                           :pairs pairs)))))
               (if (and reply (eql (getf reply :status) #.+status-ok+))
                   (incf acked)
                   (push (cons (car f)
                               (list :op #.+op-bulk-replicate+ :pairs pairs))
                         (node-outbox node)))))))
       (when (and (plusp (node-quorum node)) (< acked (node-quorum node)))
         (error "replication quorum unavailable: ~d/~d acknowledgements"
                acked (node-quorum node)))
       (list :op #.+op-ack+ :status #.+status-ok+ :seq count)))
    (#.+op-bulk-replicate+
     (with-s3-batch ()
       (dolist (pair (getf msg :pairs))
         (store-put (node-store node) (car pair) (cdr pair))))
     (list :op #.+op-ack+ :status #.+status-ok+ :seq (length (getf msg :pairs))))
    (#.+op-put+
     (node-put node (getf msg :key) (getf msg :value))
     (list :op #.+op-ack+ :status #.+status-ok+))
    (#.+op-get+
     (let ((value (store-get (node-store node) (getf msg :key))))
       (list :op #.+op-response+
             :status (if value #.+status-ok+ #.+status-not-found+)
             :value value)))
    (#.+op-delete+
     (node-delete node (getf msg :key))
     (list :op #.+op-ack+ :status #.+status-ok+))
    (#.+op-scan+
     (list :op #.+op-response+ :status #.+status-ok+
           :pairs (store-scan-fast (node-store node) (getf msg :prefix))))
    (#.+op-label-ids+
     (let ((value (%node-label-ids-summary node msg)))
       (if value
           (list :op #.+op-response+ :status #.+status-ok+
                 :value (codec-encode value))
           (list :op #.+op-response+ :status #.+status-error+
                 :value (codec-encode "label summary unavailable")))))
    (#.+op-topk+
     (let ((value (%node-topk-summary node msg)))
       (if value
           (list :op #.+op-response+ :status #.+status-ok+
                 :value (codec-encode value))
           (list :op #.+op-response+ :status #.+status-error+
                 :value (codec-encode "top-k summary unavailable")))))
    (#.+op-aggregate+
     (list :op #.+op-response+ :status #.+status-ok+
           :value (codec-encode (node-aggregate-relationships node msg))))
    (#.+op-scan-page+
     (list :op #.+op-response+ :status #.+status-ok+
           :pairs (store-scan-page (node-store node) (getf msg :prefix)
                                   (getf msg :offset) (getf msg :limit))))
    (#.+op-replicate+
     (store-apply-log-record (node-store node) msg)
     (list :op #.+op-ack+ :seq (getf msg :seq) :status #.+status-ok+))
    (#.+op-snapshot+
     (list :op #.+op-snapshot+ :pairs (store-snapshot (node-store node))))
    (#.+op-cypher+
     (handler-case
         (let* ((db (getf msg :db))
                (graph (or (gethash db (node-graphs node))
                           (setf (gethash db (node-graphs node))
                                 (make-local-graph (node-store node) :db db))))
                (params (and (plusp (length (getf msg :params)))
                             (json-decode (octets-to-string (getf msg :params)))))
                (rows (cypher-query (getf msg :query) graph :params params)))
           (list :op #.+op-response+ :status #.+status-ok+
                 :value (string-to-octets (cypher-result->json rows))))
       (error (e)
         (list :op #.+op-error+ :message (format nil "~a" e)))))
    (#.+op-ping+
     (list :op #.+op-pong+))
    (t (list :op #.+op-error+ :message (format nil "unknown opcode ~a"
                                               (getf msg :op))))))
