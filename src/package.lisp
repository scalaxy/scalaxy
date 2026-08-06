;;;; package.lisp --- Scalaxy package definition

(defpackage #:scalaxy
  (:use #:cl)
  (:shadow #:get #:delete)
  (:export
   ;; generic helpers
   #:string-to-octets #:octets-to-string #:hex-digest #:fnv1a-64
   ;; protocol / wire format
   #:+op-put+ #:+op-get+ #:+op-delete+ #:+op-scan+ #:+op-replicate+
   #:+op-ack+ #:+op-error+ #:+op-ping+ #:+op-pong+ #:+op-snapshot+
   #:+op-response+ #:+status-ok+ #:+status-not-found+
   #:encode-message #:decode-message #:frame-message #:read-frame
   ;; consistent hashing
   #:ring #:make-ring #:ring-add #:ring-remove #:ring-lookup #:ring-nodes
   #:ring-vnodes-per-node
   ;; storage
   #:store #:make-store #:store-put #:store-get #:store-delete #:store-scan
   #:store-count #:store-snapshot #:store-restore #:store-apply-log-record
   ;; node & replication
   #:node #:make-node #:node-id #:node-store #:node-put #:node-get
   #:node-delete #:node-dispatch #:node-add-follower #:node-replicate
   #:node-next-seq #:node-scan #:node-replicator
   #:replicator #:replicator-seq #:replicator-record #:replicator-entries
   ;; TCP server / client
   #:server #:tcp-serve #:tcp-stop #:server-port #:tcp-request
   ;; cluster
   #:cluster #:make-cluster #:cluster-put #:cluster-get #:cluster-delete
   #:cluster-scan #:cluster-add-node #:cluster-remove-node #:cluster-node-ids
   #:cluster-nodes #:cluster-replicas
   ;; client API
   #:client #:connect #:put #:get #:delete #:scan
   ;; entry points
   #:start-node #:main #:parse-args #:parse-host-port))
