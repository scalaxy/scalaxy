;;;; package.lisp --- Scalaxy package definition

(defpackage #:scalaxy
  (:use #:cl)
  (:shadow #:get #:delete)
  (:export
   ;; generic helpers
   #:string-to-octets #:octets-to-string #:hex-digest #:fnv1a-64 #:hash-string
   ;; databases (logical namespaces over physical keys)
   #:+default-db+ #:db-valid-name-p #:db-prefix #:db-key #:db-parse-name
   #:db-strip #:db-list #:create-database #:drop-database #:list-databases
   #:gateway-create-database #:gateway-drop-database #:gateway-list-databases
   #:cluster-create-database #:cluster-drop-database #:cluster-list-databases
   ;; value codec
   #:codec-encode #:codec-decode #:cypher-null-p #:cypher-false-p
   #:cypher-true-p #:cypher-list #:cypher-list-p #:cypher-list-elements
   #:cypher-map #:cypher-map-p #:cypher-map-pairs
   #:cypher-empty-list-p #:cypher-value= #:double-float-bits #:bits-double-float
   ;; graph storage (property graph over the KV store)
   #:graph-view #:local-graph-view #:gateway-graph-view
   #:make-local-graph #:make-gateway-graph
   #:g-put #:g-get #:g-delete #:g-scan #:g-counter
   #:graph-mint-id #:graph-create-node #:graph-create-relationship
   #:graph-node #:graph-relationship #:graph-node-property
   #:graph-relationship-property
   #:graph-set-node-property #:graph-set-relationship-property
   #:graph-remove-node-property #:graph-remove-relationship-property
   #:graph-add-node-label #:graph-remove-node-label
   #:graph-delete-node #:graph-delete-relationship
   #:graph-scan-node-ids #:graph-scan-rel-ids #:graph-expand
   #:graph-rebuild-indexes #:graph-check-invariants
   #:graph-count-nodes #:graph-count-rels #:+blob-inline-limit+
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
   #:resolve-host #:ip-string-p
   ;; JSON
   #:json-encode #:json-decode #:json-escape
   ;; HTTP server / client
   #:http-server #:http-serve #:http-stop #:http-server-port #:http-request
   #:http-url-decode #:http-parse-query #:split-sequence-on
   ;; gateway
   #:gateway #:make-gateway #:gateway-put #:gateway-get #:gateway-delete
   #:gateway-scan #:gateway-count #:gateway-status #:gateway-ring
   #:gateway-peers #:gateway-http-port #:gateway-peer-host
   #:gateway-peer-port #:gateway-peer-http-port #:gateway-request
   ;; web console
   #:make-web-handler #:node-status-plist #:run-command #:+version+
   #:json-response #:text-response #:html-response #:value-info #:preview
   ;; cluster
   #:cluster #:make-cluster #:cluster-put #:cluster-get #:cluster-delete
   #:cluster-scan #:cluster-add-node #:cluster-remove-node #:cluster-node-ids
   #:cluster-nodes #:cluster-replicas
   ;; client API
   #:client #:connect #:put #:get #:delete #:scan
   ;; cypher front end
   #:cypher-lex #:cypher-parse #:cypher-parse-expr #:ast-print #:ast-var
   #:ast-var-name #:cytoken #:cytoken-kind #:cytoken-value #:cytoken-line
   #:cytoken-col #:cypher-error #:cypher-syntax-error #:cypher-type-error
   #:cypher-argument-error #:cypher-entity-not-found #:cypher-signal
   #:cypher-error-kind #:cypher-error-detail #:cypher-error-query
   #:cypher-type-name #:cypher-= #:cypher-compare #:eval-expr
   #:expr-has-aggregate #:cypher-query #:row-get #:row-bind
   #:cursor #:cursor-next #:cursor-drain #:reference-eval
   #:cypher-check #:cypher-check-query #:cypher #:gateway-cypher
   #:cypher-value->json #:cypher-result->json #:cypher-print-value
   #:+op-cypher+
   ;; entry points
   #:start-node #:main #:parse-args #:parse-host-port))
