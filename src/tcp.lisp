;;;; tcp.lisp --- SBCL TCP server and client for Scalaxy nodes
;;;;
;;;; Frames on the wire are [4-byte big-endian length][message body].
;;;; This layer is SBCL-specific (sb-bsd-sockets / sb-thread); on other
;;;; implementations the functions signal a clear error.

(in-package #:scalaxy)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-bsd-sockets))

#-sbcl
(progn
  (defun tcp-serve (&rest args) (declare (ignore args))
    (error "Scalaxy TCP server requires SBCL."))
  (defun tcp-request (&rest args) (declare (ignore args))
    (error "Scalaxy TCP client requires SBCL.")))

#+sbcl
(progn

  (defstruct (server (:constructor %make-server (socket accept-thread threads port stopped)))
    socket
    accept-thread
    threads
    port
    stopped)  ; (list nil) -- mutable stop flag

  (defun read-frame (stream)
    "Read one length-prefixed frame from STREAM, or NIL at EOF."
    (let ((hdr (make-array 4 :element-type '(unsigned-byte 8))))
      (handler-case
          (let ((n (read-sequence hdr stream)))
            (when (= n 4)
              (let* ((len (+ (ash (aref hdr 0) 24)
                             (ash (aref hdr 1) 16)
                             (ash (aref hdr 2) 8)
                             (aref hdr 3)))
                     (body (make-array len :element-type '(unsigned-byte 8))))
                (if (= (read-sequence body stream) len)
                    body
                    nil))))
        (error () nil))))

  (defun handle-connection (node conn)
    (unwind-protect
         (let ((stream (sb-bsd-sockets:socket-make-stream
                        conn :input t :output t
                        :element-type '(unsigned-byte 8)
                        :buffering :none)))
           (loop for frame = (read-frame stream)
                 while frame
                 do (let ((reply (node-dispatch node (decode-message frame))))
                      (when reply
                        (write-sequence (frame-message reply) stream)
                        (finish-output stream)))))
      (ignore-errors (sb-bsd-sockets:socket-close conn))))

  (defun tcp-serve (node &key (host "127.0.0.1") (port 7200) (backlog 16))
    "Start a TCP server for NODE.  Returns a SERVER handle; the actual
listening port is available via SERVER-PORT (useful when PORT is 0)."
    (let* ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                  :type :stream :protocol :tcp))
           (stopped (list nil)))
      (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
      (sb-bsd-sockets:socket-bind socket
                                  (sb-bsd-sockets:make-inet-address host)
                                  port)
      (sb-bsd-sockets:socket-listen socket backlog)
      (let ((real-port (nth-value 1 (sb-bsd-sockets:socket-name socket)))
            (threads '()))
        (let ((accept-thread
                (sb-thread:make-thread
                 (lambda ()
                   (loop while (not (car stopped))
                         do (handler-case
                                (let ((conn (sb-bsd-sockets:socket-accept socket)))
                                  (push (sb-thread:make-thread
                                         (lambda () (handle-connection node conn))
                                         :name "scalaxy-conn")
                                        threads))
                              (error () (sleep 0.05)))))
                 :name "scalaxy-accept")))
          (%make-server socket accept-thread threads real-port stopped)))))

  (defun tcp-stop (server)
    (setf (car (server-stopped server)) t)
    (ignore-errors (sb-bsd-sockets:socket-close (server-socket server)))
    (ignore-errors (sb-thread:join-thread (server-accept-thread server)
                                          :timeout 2))
    server)

  (defun tcp-request (host port msg)
    "Send MSG to a Scalaxy node at HOST:PORT and return the decoded reply."
    (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream :protocol :tcp)))
      (sb-bsd-sockets:socket-connect socket
                                     (sb-bsd-sockets:make-inet-address host)
                                     port)
      (unwind-protect
           (let ((stream (sb-bsd-sockets:socket-make-stream
                          socket :input t :output t
                          :element-type '(unsigned-byte 8)
                          :buffering :none)))
             (write-sequence (frame-message msg) stream)
             (finish-output stream)
             (let ((frame (read-frame stream)))
               (when frame (decode-message frame))))
        (ignore-errors (sb-bsd-sockets:socket-close socket))))))
