;;;; gateway.lisp --- cluster gateway: routes key operations to ring owners
;;;;
;;;; A gateway keeps the cluster topology (node id -> data endpoint) and
;;;; routes each key operation over the TCP protocol to the node that owns
;;;; the key on the consistent-hash ring.  Scans fan out to every node and
;;;; merge/dedupe the results.  Status aggregation uses each node's HTTP
;;;; /api/node-status endpoint.

(in-package #:scalaxy)

(defstruct (gateway (:constructor %make-gateway (ring peers http-port)))
  ring       ; consistent-hash ring over node ids
  peers      ; alist: id -> (host data-port http-port-or-nil)
  http-port) ; default HTTP port when a peer has no explicit one

(defun make-gateway (&key (peers nil) (http-port 8080))
  "PEERS is a list of (id host data-port &optional http-port) tuples."
  (%make-gateway (make-ring :nodes (mapcar #'first peers))
                 (loop for (id host port hport) in peers
                       collect (cons id (list host port hport)))
                 http-port))

(defun gateway-peer-endpoint (gateway id)
  (cdr (assoc id (gateway-peers gateway) :test #'equal)))

(defun gateway-peer-host (gateway id)
  (first (gateway-peer-endpoint gateway id)))

(defun gateway-peer-port (gateway id)
  (second (gateway-peer-endpoint gateway id)))

(defun gateway-peer-http-port (gateway id)
  (or (third (gateway-peer-endpoint gateway id))
      (gateway-http-port gateway)))

(defun gateway-request (gateway id msg)
  (tcp-request (gateway-peer-host gateway id)
               (gateway-peer-port gateway id)
               msg))

(defun %ring-owner-order (gateway key)
  "Nodes that could hold KEY, owner first, then every other member.
Used for failover: replicas are stored on the other ring members, so
when the owner is unreachable the data can still be served."
  (let* ((owner (ring-lookup (gateway-ring gateway) key))
         (others (remove owner (mapcar #'car (gateway-peers gateway)) :test #'equal)))
    (cons owner others)))

(defun gateway-bulk-put (gateway pairs)
  "Route PAIRS to their owners using one packed request per node."
  (let ((groups (make-hash-table :test #'equal)))
    (dolist (pair pairs)
      (let ((owner (ring-lookup (gateway-ring gateway) (car pair))))
        (push pair (gethash owner groups))))
    (maphash
     (lambda (owner owner-pairs)
       (let ((reply (gateway-request gateway owner
                                     (list :op #.+op-bulk-put+
                                           :pairs (nreverse owner-pairs)))))
         (unless (and reply (eql (getf reply :status) #.+status-ok+))
           (error "Scalaxy: bulk owner ~a failed" owner))))
     groups)
    (list :status #.+status-ok+ :count (length pairs))))

(defun gateway-put (gateway key value)
  (dolist (id (%ring-owner-order gateway key))
    (when id
      (let ((reply (ignore-errors
                    (gateway-request gateway id (list :op #.+op-put+ :key key :value value)))))
        (when (and reply (eql (getf reply :status) #.+status-ok+))
          (return-from gateway-put reply)))))
  (error "Scalaxy: no reachable node for ~a" key))

(defun gateway-get (gateway key)
  (dolist (id (%ring-owner-order gateway key))
    (when id
      (let ((reply (ignore-errors
                    (gateway-request gateway id (list :op #.+op-get+ :key key)))))
        (when (and reply (eql (getf reply :status) #.+status-ok+))
          (return-from gateway-get (getf reply :value))))))
  nil)

(defun gateway-delete (gateway key)
  (dolist (id (%ring-owner-order gateway key))
    (when id
      (let ((reply (ignore-errors
                    (gateway-request gateway id (list :op #.+op-delete+ :key key)))))
        (when (and reply (eql (getf reply :status) #.+status-ok+))
          (return-from gateway-delete reply)))))
  nil)

(defun gateway-scan (gateway prefix &key (limit nil) (offset 0))
  "Scan every node, merge, dedupe and sort.  Returns a list of (key . value)."
  (let ((seen (make-hash-table :test #'equal))
        (pairs nil))
    (dolist (peer (gateway-peers gateway))
      (let* ((id (car peer))
             (reply (ignore-errors
                     (gateway-request gateway id (list :op #.+op-scan+ :prefix prefix)))))
        (when (and reply (eql (getf reply :status) #.+status-ok+))
          (dolist (p (getf reply :pairs))
            (unless (gethash (car p) seen)
              (setf (gethash (car p) seen) t)
              (push p pairs))))))
    (let ((sorted (sort pairs #'string< :key #'car)))
      (if limit
          (subseq sorted offset (min (+ offset limit) (length sorted)))
          (subseq sorted offset)))))

(defun gateway-scan-fast (gateway prefix)
  "Scan all peers without sorting or allocating a second merged list.
Callers that require deterministic order sort their final identifiers."
  (let ((seen (make-hash-table :test #'equal)) (pairs nil))
    (dolist (peer (gateway-peers gateway))
      (let ((reply (ignore-errors
                     (gateway-request gateway (car peer)
                                      (list :op #.+op-scan+ :prefix prefix)))))
        (when (and reply (eql (getf reply :status) #.+status-ok+))
          (dolist (p (getf reply :pairs))
            (unless (gethash (car p) seen)
              (setf (gethash (car p) seen) t)
              (push p pairs))))))
    pairs))

(defun gateway-graph-count (gateway kind &key (db +default-db+) label)
  "Return a fast authoritative graph count from node status metrics.
Physical replicas are divided out using the configured replica count."
  (let* ((status (gateway-status gateway :db db))
         (nodes (getf status :nodes))
         (field (if (eq kind :nodes) "graphNodes" "graphRelationships"))
         (total (if label
                    (reduce #'+ nodes
                            :key (lambda (n)
                                   (or (gethash label (gethash "graphLabelCounts" n)) 0))
                            :initial-value 0)
                    (reduce #'+ nodes :key (lambda (n) (or (gethash field n) 0)) :initial-value 0)))
         (replicas (1+ (or (and nodes (gethash "replicas" (first nodes))) 1))))
    (floor total replicas)))

(defun gateway-count (gateway)
  "Total number of distinct keys across the cluster."
  (length (gateway-scan gateway "")))

(defun gateway-create-database (gateway name)
  "Create database NAME in the cluster by writing its marker key
through the ring.  Returns the write reply."
  (unless (db-valid-name-p name)
    (error "Scalaxy: invalid database name ~s" name))
  (gateway-put gateway (db-key name "") #()))

(defun gateway-list-databases (gateway)
  "Names of all databases across the cluster, sorted."
  (mapcar #'car (db-list (gateway-scan gateway "d:"))))

(defun gateway-drop-database (gateway name)
  "Delete database NAME and every key it holds.  The implicit
\"default\" database cannot be dropped.  Returns the remaining names."
  (when (equal name +default-db+)
    (error "Scalaxy: cannot drop the implicit database \"default\""))
  (dolist (p (gateway-scan gateway (db-key name "")))
    (gateway-delete gateway (car p)))
  (gateway-list-databases gateway))

(defun %gateway-stream-aggregate (ast graph)
  "Stream simple relationship count/sum projections over primary owners."
  (when (and (consp ast) (eq (car ast) :query))
    (let* ((clauses (rest ast)) (match (first clauses)) (ret (second clauses)))
      (when (and match ret (eq (car match) :match) (eq (car ret) :return)
                 (null (cddr clauses)) (null (getf (cdr match) :where))
                 (null (getf (cdr ret) :distinct)) (null (getf (cdr ret) :order))
                 (= (length (getf (cdr ret) :items)) 1))
        (let* ((item (first (getf (cdr ret) :items)))
               (expr (getf (cdr item) :expr))
               (pattern (getf (cdr match) :pattern))
               (chain (and (= (length pattern) 1) (first pattern)))
               (rel (and (= (length chain) 3) (second chain)))
               (rtype (and (consp rel) (getf (cdr rel) :type))))
          (when (and rel (eq (car rel) :rel)
                     (null (getf (cdr rel) :min)) (null (getf (cdr rel) :max))
                     (or (and (consp expr) (eq (car expr) :count-*))
                         (and (consp expr) (string-equal (getf (cdr expr) :fn) "count")
                              (= (length (getf (cdr expr) :args)) 1))
                         (and (consp expr) (string-equal (getf (cdr expr) :fn) "sum")
                              (= (length (getf (cdr expr) :args)) 1))))
            (let* ((count 0) (sum 0) (sum-seen nil)
                   (arg (and (consp expr) (first (getf (cdr expr) :args))))
                  (prop (and (consp arg) (eq (car arg) :prop)
                             (getf (cdr arg) :prop)))
                  (graph-rel graph))
              (graph-stream-relationships
               graph-rel
               (lambda (r)
                 (incf count)
                 (when prop
                   (let ((v (cdr (assoc prop (getf r :props) :test #'equal))))
                     (unless (or (null v) (eq v :cypher-null))
                       (incf sum v) (setf sum-seen t)))))
               :type rtype)
              (let ((value (if (or (eq (car expr) :count-*)
                                   (and (consp expr)
                                        (string-equal (getf (cdr expr) :fn) "count")))
                               count
                               (if sum-seen sum :cypher-null))))
                (list (list (cons (%item-name item) value)))))))))))

(defun %gateway-node-count (ast graph)
  (when (and (consp ast) (eq (car ast) :query))
    (let* ((clauses (rest ast)) (match (first clauses)) (ret (second clauses))
           (pattern (and match (getf (cdr match) :pattern)))
           (chain (and pattern (= (length pattern) 1) (first pattern)))
           (item (and ret (eq (car ret) :return) (first (getf (cdr ret) :items))))
           (expr (and item (getf (cdr item) :expr))))
      (when (and match ret (eq (car match) :match) (eq (car ret) :return)
                 (null (cddr clauses)) (null (getf (cdr match) :where))
                 (= (length (getf (cdr ret) :items)) 1)
                 (consp chain) (= (length chain) 1) (eq (car (first chain)) :node)
                 (consp expr)
                 (or (eq (car expr) :count-*)
                     (and (eq (car expr) :call)
                          (string-equal (getf (cdr expr) :fn) "count"))))
        (let ((labels (getf (cdr (first chain)) :labels)))
          (when (= (length labels) 1)
            (list (list (cons (%item-name item)
                              (gateway-graph-count (graph-gateway graph) :nodes
                                                   :db (graph-db graph)
                                                   :label (first labels)))))))))))

(defun gateway-aggregate-relationships (gateway db type function property &key (left-label "") (right-label "") left-ids right-ids)
  "Push a scalar relationship aggregate to every node and remove replicas."
  (let ((total (if (string-equal function "COUNT") 0 0.0))
        (seen nil) (healthy 0))
    (dolist (peer (gateway-peers gateway))
      (let ((reply (ignore-errors
                     (gateway-request gateway (car peer)
                                      (list :op #.+op-aggregate+
                                            :prefix (db-key db "") :type type
                                            :property property :function function
                                            :left-label left-label :right-label right-label
                                            :left-ids left-ids :right-ids right-ids)))))
        (when (and reply (eql (getf reply :status) #.+status-ok+))
          (incf healthy)
          (let ((value (car (multiple-value-list
                             (codec-decode (getf reply :value))))))
            (incf total value)))))
    (unless (plusp healthy) (error "no nodes available for aggregate"))
    ;; Each key is written to its primary plus one synchronous replica,
    ;; so every live key appears on exactly two nodes.  Divide by two
    ;; to get the cluster-wide unique aggregate.
    (let ((replicas 2))
      (let ((result (if (string-equal function "COUNT")
                        (floor total replicas)
                        (/ total replicas))))
        result))))

(defun %gateway-pushdown-aggregate (ast graph)
  (when (and (consp ast) (eq (car ast) :query))
    (let* ((clauses (rest ast)) (match (first clauses)) (ret (second clauses))
           (pattern (and match (getf (cdr match) :pattern)))
           (chain (and pattern (= (length pattern) 1) (first pattern)))
           (item (and ret (eq (car ret) :return)
                      (first (getf (cdr ret) :items))))
           (expr (and item (getf (cdr item) :expr)))
           (rel (and chain (= (length chain) 3) (second chain)))
           (left (and chain (= (length chain) 3) (first chain)))
           (right (and chain (= (length chain) 3) (third chain))))
      (when (and match ret (eq (car match) :match)
                 (null (cddr clauses)) (null (getf (cdr match) :where))
                 (null (getf (cdr ret) :distinct)) (null (getf (cdr ret) :order))
                 rel (eq (car rel) :rel)
                 left right (eq (car left) :node) (eq (car right) :node)
                 (<= (length (getf (cdr left) :labels)) 1)
                 (<= (length (getf (cdr right) :labels)) 1)
                 (null (getf (cdr left) :props))
                 (null (getf (cdr right) :props))
                 (eq (getf (cdr rel) :dir) :out)
                 (consp expr)
                 (or (eq (car expr) :count-*)
                     (and (eq (car expr) :call)
                          (member (string-upcase (getf (cdr expr) :fn)) '("COUNT" "SUM")
                                  :test #'string=))))
        (let* ((function (if (eq (car expr) :count-*) "COUNT"
                             (string-upcase (getf (cdr expr) :fn))))
               (property (if (string= function "SUM")
                             (let ((arg (first (getf (cdr expr) :args))))
                               (and (consp arg) (getf (cdr arg) :prop)))
                             ""))
               (value (gateway-aggregate-relationships
                       (graph-gateway graph) (graph-db graph)
                       (getf (cdr rel) :type) function property
                       :left-label (or (first (getf (cdr left) :labels)) "")
                       :right-label (or (first (getf (cdr right) :labels)) "")
                       :left-ids (and (first (getf (cdr left) :labels))
                                      (format nil "~{~a~^,~}" (graph-scan-node-ids
                                                              graph :label (first (getf (cdr left) :labels)))))
                       :right-ids (and (first (getf (cdr right) :labels))
                                       (format nil "~{~a~^,~}" (graph-scan-node-ids
                                                               graph :label (first (getf (cdr right) :labels))))))))
          (list (list (cons (%item-name item) value))))))))

(defun %gateway-limited-relationship-projection (ast graph)
  "Stream a bounded scalar relationship projection for LIMIT queries."
  (when (and (consp ast) (eq (car ast) :query))
    (let* ((clauses (rest ast)) (match (first clauses)) (ret (second clauses))
           (lim (third clauses)) (pattern (and match (getf (cdr match) :pattern)))
           (chain (and pattern (= (length pattern) 1) (first pattern)))
           (left (and chain (= (length chain) 3) (first chain)))
           (rel (and chain (= (length chain) 3) (second chain)))
           (right (and chain (= (length chain) 3) (third chain)))
           (item (and ret (eq (car ret) :return)
                      (= (length (getf (cdr ret) :items)) 1)
                      (first (getf (cdr ret) :items))))
           (expr (and item (getf (cdr item) :expr)))
           (limit-expr (and lim (eq (car lim) :limit)
                            (getf (cdr lim) :expr)))
           (limit (and (consp limit-expr) (eq (car limit-expr) :lit)
                       (second limit-expr))))
      (when (and match ret (eq (car match) :match) (eq (car ret) :return)
                 (null (getf (cdr match) :where))
                 (null (getf (cdr ret) :distinct)) (null (getf (cdr ret) :order))
                 (integerp limit) (plusp limit)
                 left right (eq (car left) :node) (eq (car right) :node)
                 (null (getf (cdr left) :labels)) (null (getf (cdr right) :labels))
                 (null (getf (cdr left) :props)) (null (getf (cdr right) :props))
                 rel (eq (car rel) :rel) (eq (getf (cdr rel) :dir) :out)
                 (null (getf (cdr rel) :min)) (null (getf (cdr rel) :max))
                 (consp expr) (eq (car expr) :prop)
                 (equal (getf (cdr expr) :expr) (getf (cdr rel) :var)))
        (let ((rows nil) (property (getf (cdr expr) :prop)))
          (graph-stream-relationships
           graph
           (lambda (r)
             (push (list (cons (%item-name item)
                               (cdr (assoc property (getf r :props) :test #'equal)))) rows))
           :type (getf (cdr rel) :type) :limit limit)
          (nreverse rows))))))

(defun %gateway-topk-summary (graph prefix type property limit descending)
  (let ((gw (graph-gateway graph)) (seen (make-hash-table :test #'equal)) (values nil)
        (healthy 0))
    (dolist (peer (gateway-peers gw))
      (let* ((id (car peer))
             (reply (ignore-errors
                      (gateway-request gw id
                                       (list :op #.+op-topk+ :prefix prefix :type type
                                             :property property :limit limit
                                             :descending descending)))))
        (when (and reply (eql (getf reply :status) #.+status-ok+))
          (incf healthy)
          (let ((local (car (multiple-value-list (codec-decode (getf reply :value))))))
            (dolist (entry local)
              (let ((rid (first entry)) (value (second entry)))
                (unless (gethash rid seen)
                  (setf (gethash rid seen) t)
                  (push (cons value rid) values))))))))
    (when (plusp healthy)
      (let ((ordered (sort values (if descending #'> #'<) :key #'car)))
        (subseq ordered 0 (min limit (length ordered)))))))

(defun %gateway-topk-query (ast graph)
  (when (and (consp ast) (eq (car ast) :query))
    (let* ((clauses (rest ast)) (match (first clauses)) (ret (second clauses))
           (ord (getf (cdr ret) :order)) (lim (third clauses))
           (pattern (and match (getf (cdr match) :pattern)))
           (chain (and pattern (= (length pattern) 1) (first pattern)))
           (left (and chain (first chain))) (rel (and chain (second chain)))
           (right (and chain (third chain)))
           (item (and ret (eq (car ret) :return)
                      (= (length (getf (cdr ret) :items)) 1)
                      (first (getf (cdr ret) :items))))
           (expr (and item (getf (cdr item) :expr)))
           (spec (and ord (= (length ord) 1) (first ord)))
           (lexpr (and lim (eq (car lim) :limit) (getf (cdr lim) :expr)))
           (limit (and (consp lexpr) (eq (car lexpr) :lit) (second lexpr))))
      (when (and match ret ord lim (eq (car match) :match)
                 (null (getf (cdr match) :where)) (null (getf (cdr ret) :distinct))
                 left right (eq (car left) :node) (eq (car right) :node)
                 (null (getf (cdr left) :labels)) (null (getf (cdr right) :labels))
                 (null (getf (cdr left) :props)) (null (getf (cdr right) :props))
                 rel (eq (car rel) :rel) (eq (getf (cdr rel) :dir) :out)
                 (null (getf (cdr rel) :min)) (null (getf (cdr rel) :max))
                 (integerp limit) (plusp limit) (<= limit 100) spec
                 (equal (getf (cdr spec) :expr) (getf (cdr item) :as))
                 (consp expr) (eq (car expr) :prop)
                 (equal (getf (cdr expr) :expr) (getf (cdr rel) :var)))
        (let* ((prefix (db-key (graph-db graph) ""))
               (values (%gateway-topk-summary graph prefix (getf (cdr rel) :type)
                                               (getf (cdr expr) :prop) limit
                                               (getf (cdr spec) :desc))))
          (when values
            (mapcar (lambda (p) (list (cons (getf (cdr item) :as) (car p)))) values)))))))

(defun gateway-cypher (gateway query &key (db +default-db+) params)
  "Evaluate a Cypher QUERY across the cluster: the executor runs on the
coordinator with a gateway graph-view, so every scan/expand/point-read
routes to the ring owners with the existing failover order; writes go
through the normal replicated path (plan section 10, Phase A)."
  (let* ((graph (make-gateway-graph gateway :db db))
         (ast (cypher-parse query)))
    (or (%gateway-node-count ast graph)
        (%gateway-limited-relationship-projection ast graph)
        (%gateway-topk-query ast graph)
        (%gateway-pushdown-aggregate ast graph)
        (%gateway-stream-aggregate ast graph)
        (%fast-count-query ast graph)
        (cypher-query ast graph :params params))))

(defun gateway-status (gateway &key (db +default-db+))
  "Aggregate per-node status over each node's HTTP endpoint.
Returns a plist with :nodes (list of status plists) and :total-keys."
  (let ((nodes nil)
        (total 0))
    (dolist (peer (gateway-peers gateway))
      (let* ((id (car peer))
             (host (gateway-peer-host gateway id))
             (reply (ignore-errors
                     (multiple-value-bind (status hdrs body)
                         (http-request host (gateway-peer-http-port gateway id)
                                       "GET" (format nil "/api/node-status?db=~a" db))
                       (declare (ignore hdrs))
                       (when (and (= status 200) body)
                         (json-decode body))))))
        (if reply
            (progn
              (setf (gethash "id" reply) id)
              (push reply nodes)
              (incf total (or (gethash "keys" reply) 0)))
            (push (let ((h (make-hash-table :test #'equal)))
                    (setf (gethash "id" h) id
                          (gethash "status" h) "unreachable")
                    h)
                  nodes))))
    (list :nodes (nreverse nodes) :total-keys total)))
