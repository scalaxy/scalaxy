;;;; main.lisp --- standalone node entry point

(in-package #:scalaxy)

(defun parse-args (args)
  "Parse --address HOST:PORT, --data-dir DIR and --id ID from ARGS."
  (let ((address "127.0.0.1:7200")
        (data-dir "./scalaxy-data")
        (id nil))
    (loop for (k v) on args by #'cddr
          do (cond ((string= k "--address") (setf address v))
                   ((string= k "--data-dir") (setf data-dir v))
                   ((string= k "--id") (setf id v))))
    (values address data-dir id)))

(defun parse-host-port (address)
  (let ((pos (position #\: address)))
    (unless pos (error "invalid address ~a (expected HOST:PORT)" address))
    (values (subseq address 0 pos)
            (parse-integer (subseq address (1+ pos))))))

(defun start-node (&key (id nil) (address "127.0.0.1:7200") (data-dir "./scalaxy-data"))
  "Start a Scalaxy node: durable store + TCP server.  Returns the SERVER
handle.  Run inside a loop for a daemon (see MAIN)."
  (multiple-value-bind (host port) (parse-host-port address)
    (let* ((node-id (or id (format nil "node-~d" (get-universal-time))))
           (node (make-node
                  :id node-id
                  :store (make-store
                          :path (merge-pathnames
                                 (format nil "~a.log" node-id)
                                 (uiop:ensure-directory-pathname data-dir))))))
      (let ((server (tcp-serve node :host host :port port)))
        (format t "~&Scalaxy node ~a listening on ~a:~d (data-dir ~a)~%"
                node-id host (server-port server) data-dir)
        (finish-output)
        server))))

(defun main (&rest argv)
  "Entry point for the standalone node.  ARGV is the command-line tail
(e.g. (\"--address\" \"127.0.0.1:7200\" \"--data-dir\" \"./data\"))."
  (multiple-value-bind (address data-dir id) (parse-args argv)
    (start-node :id id :address address :data-dir data-dir)
    (loop (sleep 3600))))
