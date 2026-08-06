;;;; http.lisp --- minimal HTTP/1.1 server and client (SBCL, no dependencies)
;;;;
;;;; Request plist passed to the handler:
;;;;   :method :path :query (alist) :headers (alist) :body (string or nil)
;;;; Response plist returned by the handler:
;;;;   :status (integer, default 200) :headers (alist) :body (string or octets)
;;;;
;;;; All stream I/O is byte-level (socket-make-stream with an
;;;; (unsigned-byte 8) element type), so no external libraries are needed.

(in-package #:scalaxy)

#-sbcl
(progn
  (defun http-serve (&rest args) (declare (ignore args))
    (error "Scalaxy HTTP server requires SBCL."))
  (defun http-request (&rest args) (declare (ignore args))
    (error "Scalaxy HTTP client requires SBCL.")))

#+sbcl
(progn

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (require :sb-bsd-sockets))

  (defstruct (http-server (:constructor %make-http-server (socket accept-thread stopped)))
    socket
    accept-thread
    stopped)  ; (list nil)

  (defun http-url-decode (string)
    "Decode percent-encoding and '+' as space."
    (let ((out (make-string-output-stream)))
      (loop with i = 0
            while (< i (length string))
            do (let ((ch (char string i)))
                 (cond ((char= ch #\%)
                        (let ((code (parse-integer string :start (1+ i) :end (+ i 3) :radix 16)))
                          (write-char (code-char code) out)
                          (incf i 3)))
                       ((char= ch #\+) (write-char #\Space out) (incf i))
                       (t (write-char ch out) (incf i)))))
      (get-output-stream-string out)))

  (defun split-sequence-on (delimiter string)
    (let ((parts nil) (start 0))
      (loop for i from 0 below (length string)
            do (when (char= (char string i) delimiter)
                 (push (subseq string start i) parts)
                 (setf start (1+ i))))
      (nreverse (cons (subseq string start) parts))))

  (defun http-parse-query (query)
    "Parse a query string into an alist of (key . value)."
    (let ((pairs nil))
      (dolist (part (split-sequence-on #\& query))
        (let ((eq (position #\= part)))
          (if eq
              (push (cons (http-url-decode (subseq part 0 eq))
                          (http-url-decode (subseq part (1+ eq))))
                    pairs)
              (push (cons (http-url-decode part) "") pairs))))
      (nreverse pairs)))

  (defun read-line-bytes (stream)
    "Read one CRLF/LF-terminated line from a byte stream as a string.
Returns NIL at end of stream with no pending data."
    (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                             :adjustable t :fill-pointer 0)))
      (loop
        (let ((b (read-byte stream nil nil)))
          (cond ((null b) (return (when (plusp (length out))
                                    (octets-to-string out))))
                ((= b 10) (return (octets-to-string out)))
                ((/= b 13) (vector-push-extend b out)))))))

  (defun http-parse-request (stream)
    "Read one HTTP request from STREAM.  Returns a request plist or NIL at EOF."
    (handler-case
        (let ((request-line (read-line-bytes stream)))
          (when request-line
            (let* ((parts (split-sequence-on #\Space request-line))
                   (method (first parts))
                   (target (second parts))
                   (qpos (position #\? target)))
              (unless method (error "bad request line"))
              (let ((path (if qpos (subseq target 0 qpos) target))
                    (query (if qpos (http-parse-query (subseq target (1+ qpos))) nil))
                    (headers nil))
                (loop for line = (read-line-bytes stream)
                      while (and line (plusp (length line)))
                      do (let ((colon (position #\: line)))
                           (when colon
                             (push (cons (string-downcase (string-trim " " (subseq line 0 colon)))
                                         (string-trim " " (subseq line (1+ colon))))
                                   headers))))
                (let* ((header-alist (nreverse headers))
                       (content-length (parse-integer (or (cdr (assoc "content-length" header-alist :test #'equal)) "0")
                                                      :junk-allowed t)))
                  (let ((body (when (plusp content-length)
                                (let ((buf (make-array content-length :element-type '(unsigned-byte 8))))
                                  (read-sequence buf stream)
                                  (octets-to-string buf)))))
                    (list :method method :path path :query query
                          :headers header-alist :body body)))))))
      (error () nil)))

  (defun http-status-text (code)
    (or (cdr (assoc code '((200 . "OK") (201 . "Created") (204 . "No Content")
                           (400 . "Bad Request") (404 . "Not Found")
                           (405 . "Method Not Allowed") (409 . "Conflict")
                           (415 . "Unsupported Media Type") (500 . "Internal Server Error")
                           (503 . "Service Unavailable"))))
        "Unknown"))

  (defun http-write-response (stream response)
    (let* ((status (or (getf response :status) 200))
           (body (or (getf response :body) ""))
           (body-octets (etypecase body
                          (string (string-to-octets body))
                          ((vector (unsigned-byte 8)) body)))
           (headers (getf response :headers))
           (head (with-output-to-string (out)
                   (format out "HTTP/1.1 ~d ~a~%" status (http-status-text status))
                   (format out "Content-Length: ~d~%" (length body-octets))
                   (dolist (h headers)
                     (format out "~a: ~a~%" (car h) (cdr h)))
                   (write-string "Connection: close" out)
                   (format out "~%~%")))
           (head-octets (string-to-octets head)))
      (write-sequence head-octets stream)
      (write-sequence body-octets stream)
      (finish-output stream)))

  (defun http-handle-connection (handler conn)
    (unwind-protect
         (let ((stream (sb-bsd-sockets:socket-make-stream
                        conn :input t :output t
                        :element-type '(unsigned-byte 8)
                        :buffering :none)))
           (let ((request (http-parse-request stream)))
             (when request
               (let ((response (handler-case (funcall handler request)
                                 (error (e)
                                   (list :status 500
                                         :headers (list (cons "Content-Type" "application/json"))
                                         :body (json-encode (list (cons "error" (princ-to-string e)))))))))
                 (http-write-response stream response)))))
      (ignore-errors (sb-bsd-sockets:socket-close conn))))

  (defun http-serve (handler &key (host "127.0.0.1") (port 8080) (backlog 64))
    "Start an HTTP server; HANDLER is called with a request plist and must
return a response plist.  Returns an HTTP-SERVER handle; the actual port is
available via (http-server-port SERVER)."
    (let* ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                  :type :stream :protocol :tcp))
           (stopped (list nil)))
      (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
      (sb-bsd-sockets:socket-bind socket
                                  (sb-bsd-sockets:make-inet-address host)
                                  port)
      (sb-bsd-sockets:socket-listen socket backlog)
      (let ((real-port (nth-value 1 (sb-bsd-sockets:socket-name socket))))
        (declare (ignore real-port))
        (let ((accept-thread
                (sb-thread:make-thread
                 (lambda ()
                   (loop while (not (car stopped))
                         do (handler-case
                                (let ((conn (sb-bsd-sockets:socket-accept socket)))
                                  (sb-thread:make-thread
                                   (lambda () (http-handle-connection handler conn))
                                   :name "scalaxy-http"))
                              (error () (sleep 0.05)))))
                 :name "scalaxy-http-accept")))
          (%make-http-server socket accept-thread stopped)))))

  (defun http-server-port (server)
    (nth-value 1 (sb-bsd-sockets:socket-name (http-server-socket server))))

  (defun http-stop (server)
    (setf (car (http-server-stopped server)) t)
    (ignore-errors (sb-bsd-sockets:socket-close (http-server-socket server)))
    (ignore-errors (sb-thread:join-thread (http-server-accept-thread server)
                                          :timeout 2))
    server)

  (defun http-request (host port method path &key (headers nil) (body nil))
    "Issue an HTTP request and return (VALUES STATUS HEADER-ALIST BODY-STRING)."
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
             (let* ((body-octets (when body
                                   (etypecase body
                                     (string (string-to-octets body))
                                     ((vector (unsigned-byte 8)) body))))
                    (head (with-output-to-string (out)
                            (format out "~a ~a HTTP/1.1~%Host: ~a:~d~%Connection: close~%"
                                    method path host port)
                            (when body-octets
                              (format out "Content-Length: ~d~%" (length body-octets)))
                            (dolist (h headers)
                              (format out "~a: ~a~%" (car h) (cdr h)))
                            (format out "~%")))
                    (head-octets (string-to-octets head)))
               (write-sequence head-octets stream)
               (when body-octets
                 (write-sequence body-octets stream))
               (finish-output stream)
               ;; --- read response ---
               (let ((status-line (read-line-bytes stream)))
                 (unless status-line
                   (return-from http-request (values 0 nil "")))
                 (let* ((status (parse-integer (second (split-sequence-on #\Space status-line))
                                               :junk-allowed t))
                        (headers nil))
                   (loop for line = (read-line-bytes stream)
                         while (and line (plusp (length line)))
                         do (let ((colon (position #\: line)))
                              (when colon
                                (push (cons (string-downcase (string-trim " " (subseq line 0 colon)))
                                            (string-trim " " (subseq line (1+ colon))))
                                      headers))))
                   (let ((len (parse-integer (or (cdr (assoc "content-length" headers :test #'equal)) "0")
                                             :junk-allowed t)))
                     (let ((body-str (when (plusp len)
                                       (let ((buf (make-array len :element-type '(unsigned-byte 8))))
                                         (read-sequence buf stream)
                                         (octets-to-string buf)))))
                       (values status (nreverse headers) body-str)))))))
        (ignore-errors (sb-bsd-sockets:socket-close socket))))))
