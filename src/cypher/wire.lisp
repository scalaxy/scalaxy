;;;; wire.lisp --- Cypher result serialization to JSON (dependency-free)
;;;;
;;;; Converts Cypher values to JSON for the wire and the REST API:
;;;; nodes/relationships/paths become tagged objects, byte blobs become
;;;; {"~blob":"<hex>"} objects, scalars map 1:1 (false/null/true kept
;;;; distinct).

(in-package #:scalaxy)

(defun %cypher->jsonable (v)
  "Convert a Cypher value to a JSON-encodable CL structure."
  (cond
    ((cypher-null-p v) :cypher-null)
    ((eq v t) t)
    ((cypher-false-p v) :cypher-false)
    ((stringp v) v)
    ((integerp v) v)
    ((floatp v) v)
    ((typep v '(vector (unsigned-byte 8)))
     (list (cons "~blob" (hex-digest v))))
    ((%node-p v)
     (list (cons "~t" "node")
           (cons "id" (getf v :id))
           (cons "labels" (getf v :labels))
           (cons "props" (cypher-map-pairs (cypher-map (getf v :props))))))
    ((%rel-p v)
     (list (cons "~t" "rel")
           (cons "id" (getf v :id))
           (cons "type" (getf v :type))
           (cons "start" (getf v :start))
           (cons "end" (getf v :end))
           (cons "props" (cypher-map-pairs (cypher-map (getf v :props))))))
    ((%path-p v)
     (list (cons "~t" "path")
           (cons "nodes" (loop for (n r) on (second v) by #'cddr
                               collect (%cypher->jsonable n)))
           (cons "rels" (loop for (n r) on (cdr (second v)) by #'cddr
                              collect (%cypher->jsonable r)))))
    ((cypher-map-p v)
     (list (cons "~t" "map")
           (cons "entries" (mapcar (lambda (p)
                                     (list (cons "k" (car p))
                                           (cons "v" (%cypher->jsonable (cdr p)))))
                                   (cypher-map-pairs v)))))
    ((cypher-list-p v)
     (mapcar #'%cypher->jsonable (cypher-list-elements v)))
    (t (princ-to-string v))))

(defun %json-emit (x out)
  "Emit the JSON encoding of X (produced by %CYPHER->JSONABLE)."
  (cond
    ((null x) (write-string "null" out))
    ((eq x :cypher-null) (write-string "null" out))
    ((eq x t) (write-string "true" out))
    ((eq x :cypher-false) (write-string "false" out))
    ((stringp x)
     (write-char #\" out)
     (write-string (json-escape x) out)
     (write-char #\" out))
    ((integerp x) (format out "~d" x))
    ((floatp x) (format out "~a" x))
    ((and (consp x) (every #'consp x))
     ;; object (alist of pairs) or array of objects?
     (if (and x (stringp (caar x)))
         (progn
           (write-char #\{ out)
           (loop for p in x for first = t then nil
                 do (unless first (write-char #\, out))
                    (write-char #\" out)
                    (write-string (json-escape (car p)) out)
                    (write-char #\" out)
                    (write-char #\: out)
                    (%json-emit (cdr p) out))
           (write-char #\} out))
         (progn
           (write-char #\[ out)
           (loop for item in x for first = t then nil
                 do (unless first (write-char #\, out))
                    (%json-emit item out))
           (write-char #\] out))))
    ((consp x)
     (write-char #\[ out)
     (loop for item in x for first = t then nil
           do (unless first (write-char #\, out))
              (%json-emit item out))
     (write-char #\] out))
    (t (write-string (princ-to-string x) out))))

(defun cypher-value->json (v)
  (with-output-to-string (out) (%json-emit (%cypher->jsonable v) out)))

(defun cypher-result->json (rows)
  "Serialize query result ROWS (alists) as
{\"columns\":[...],\"rows\":[[...]],\"count\":n}."
  (let ((columns (mapcar #'car (first rows))))
    (with-output-to-string (out)
      (write-string "{\"columns\":[" out)
      (loop for c in columns for first = t then nil
            do (unless first (write-char #\, out))
               (write-char #\" out)
               (write-string (json-escape (symbol-name c)) out)
               (write-char #\" out))
      (write-string "],\"rows\":[" out)
      (loop for row in rows for first = t then nil
            do (unless first (write-char #\, out))
               (write-char #\[ out)
               (loop for c in columns for first2 = t then nil
                     do (unless first2 (write-char #\, out))
                        (%json-emit (%cypher->jsonable (cdr (assoc c row))) out))
               (write-char #\] out))
      (format out "],\"count\":~d}" (length rows)))))

(defun cypher-print-value (v)
  "Human-readable single-line rendering of a Cypher value."
  (cond
    ((cypher-null-p v) "null")
    ((eq v t) "true")
    ((cypher-false-p v) "false")
    ((stringp v) (format nil "'~a'" v))
    ((numberp v) (format nil "~a" v))
    ((typep v '(vector (unsigned-byte 8))) (format nil "blob(~a bytes)" (length v)))
    ((%node-p v) (format nil "(:~{~a~^:~} {~{~a~^, ~}})"
                         (getf v :labels)
                         (mapcar (lambda (p) (format nil "~a: ~a" (car p) (cypher-print-value (cdr p))))
                                 (getf v :props))))
    ((%rel-p v) (format nil "[:~a]" (getf v :type)))
    ((%path-p v) (format nil "<path ~d nodes>" (/ (1+ (length (second v))) 2)))
    ((cypher-map-p v)
     (format nil "{~{~a~^, ~}}"
             (mapcar (lambda (p) (format nil "~a: ~a" (car p) (cypher-print-value (cdr p))))
                     (cypher-map-pairs v))))
    ((cypher-list-p v)
     (format nil "[~{~a~^, ~}]" (mapcar #'cypher-print-value (cypher-list-elements v))))
    (t (princ-to-string v))))
