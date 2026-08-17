;;;; functions.lisp --- Cypher expressions: evaluator + function library
;;;;
;;;; Pure: (EVAL-EXPR expr row graph params) -> Cypher value.  Follows
;;;; openCypher 9 semantics: three-valued logic (Kleene tables), NULL
;;;; propagation, numeric cross-type equality, 0-based list indexing.

(in-package #:scalaxy)

;;; ------------------------------------------------------------------
;;; three-valued logic

(defun %tv-true (v) (eq v t))
(defun %tv-false (v) (cypher-false-p v))
(defun %tv-null (v) (cypher-null-p v))

(defun %tv-not (v)
  (cond ((%tv-null v) :cypher-null)
        ((%tv-true v) :cypher-false)
        ((cypher-false-p v) t)
        (t (cypher-signal "InvalidArgumentType"
                          :detail (format nil "NOT on non-boolean ~a"
                                          (cypher-type-name v))))))

(defun %tv-bool-p (v)
  (or (cypher-null-p v) (eq v t) (cypher-false-p v)))

(defun %tv-check-bool (v)
  (unless (%tv-bool-p v)
    (cypher-signal "InvalidArgumentType"
                   :detail (format nil "expected a boolean, got ~a" (cypher-type-name v)))))

(defun %tv-and (a b)
  (%tv-check-bool a) (%tv-check-bool b)
  (cond ((or (%tv-false a) (%tv-false b)) :cypher-false)
        ((or (%tv-null a) (%tv-null b)) :cypher-null)
        (t t)))

(defun %tv-or (a b)
  (%tv-check-bool a) (%tv-check-bool b)
  (cond ((or (%tv-true a) (%tv-true b)) t)
        ((or (%tv-null a) (%tv-null b)) :cypher-null)
        (t :cypher-false)))

(defun %tv-xor (a b)
  (%tv-check-bool a) (%tv-check-bool b)
  (cond ((or (%tv-null a) (%tv-null b)) :cypher-null)
        (t (if (eq (%tv-true a) (%tv-true b)) :cypher-false t))))

;;; ------------------------------------------------------------------
;;; typing and equality

(defun cypher-type-name (v)
  (cond ((%tv-null v) "NULL")
        ((or (eq v t) (cypher-false-p v)) "BOOLEAN")
        ((integerp v) "INTEGER")
        ((floatp v) "FLOAT")
        ((stringp v) "STRING")
        ((typep v '(vector (unsigned-byte 8))) "BLOB")
        ((cypher-map-p v) "MAP")
        ((%rel-p v) "RELATIONSHIP")
        ((%node-p v) "NODE")
        ((cypher-list-p v) "LIST")
        ((and (consp v) (eq (car v) :path)) "PATH")
        (t "ANY")))

(defun %entity-plist-p (v)
  "True when V is a node/relationship/path value (a plist that must
not be confused with a list)."
  (and (consp v)
       (null (cdr (last v)))
       (evenp (length v))
       (or (and (getf v :id)
                (not (eq (getf v :labels :missing) :missing))) ; node
           (and (getf v :id) (getf v :type))                   ; relationship
           (eq (car v) :path))))                               ; path

(defun %node-p (v)
  (and (%entity-plist-p v) (not (cypher-map-p v))
       (getf v :id) (not (getf v :type))))

(defun %rel-p (v)
  (and (%entity-plist-p v) (not (cypher-map-p v))
       (getf v :id) (getf v :type)))
(defun %path-p (v) (and (consp v) (eq (car v) :path)))

(defun cypher-= (a b)
  "Cypher equality of expressions: numbers compare numerically across
int/float; NULL equals nothing (yields NULL, handled by callers).
Boolean results use the T/:CYPHER-FALSE convention, never CL NIL."
  (flet ((b (x) (if x t :cypher-false)))
    (cond
      ((or (%tv-null a) (%tv-null b)) :cypher-null)
      ((and (numberp a) (numberp b)) (b (= a b)))
      ((and (stringp a) (stringp b)) (b (string= a b)))
      ((and (or (eq a t) (cypher-false-p a))
            (or (eq b t) (cypher-false-p b)))
       (b (eq (%tv-true a) (%tv-true b))))
      ((and (%node-p a) (%node-p b)) (b (equal (getf a :id) (getf b :id))))
      ((and (%rel-p a) (%rel-p b)) (b (equal (getf a :id) (getf b :id))))
      ((and (cypher-map-p a) (cypher-map-p b))
       (let ((as (cypher-map-pairs a)) (bs (cypher-map-pairs b)))
         (if (/= (length as) (length bs))
             :cypher-false
             (let ((seen-null nil))
               (dolist (p as)
                 (let ((q (assoc (car p) bs :test #'equal)))
                   (cond ((null q) (return-from cypher-= :cypher-false))
                         (t (let ((e (cypher-= (cdr p) (cdr q))))
                              (cond ((%tv-null e) (setf seen-null t))
                                    ((%tv-true e))
                                    (t (return-from cypher-= :cypher-false))))))))
               (if seen-null :cypher-null t)))))
      ((and (cypher-list-p a) (cypher-list-p b))
       (let ((as (cypher-list-elements a)) (bs (cypher-list-elements b)))
         (if (/= (length as) (length bs))
             :cypher-false
             (let ((seen-null nil))
               (dolist (x as)
                 (let ((e (cypher-= x (pop bs))))
                   (cond ((%tv-null e) (setf seen-null t))
                         ((%tv-true e))
                         (t (return-from cypher-= :cypher-false)))))
               (if seen-null :cypher-null t)))))
      (t :cypher-false))))

(defun cypher-compare (a b)
  "Total order for ORDER BY and < <= > >=.  Returns :lt :eq :gt or
:null when the values are incomparable."
  (cond
    ((or (%tv-null a) (%tv-null b)) :null)
    ((and (numberp a) (numberp b))
     (cond ((< a b) :lt) ((> a b) :gt) (t :eq)))
    ((and (stringp a) (stringp b))
     (cond ((string< a b) :lt) ((string> a b) :gt) (t :eq)))
    ((and (or (eq a t) (cypher-false-p a))
          (or (eq b t) (cypher-false-p b)))
     (let ((x (%tv-true a)) (y (%tv-true b)))
       (cond ((and x (not y)) :gt) ((and (not x) y) :lt) (t :eq))))
    ((and (cypher-list-p a) (cypher-list-p b))
     (let ((as (cypher-list-elements a)) (bs (cypher-list-elements b)))
       (loop for x in as for y in bs
             for c = (cypher-compare x y)
             do (when (member c '(:lt :gt)) (return c))
             finally (return (cond ((< (length as) (length bs)) :lt)
                                   ((> (length as) (length bs)) :gt)
                                   (t :eq))))))
    (t :null)))

;;; ------------------------------------------------------------------
;;; scalar function library

(defun %fn-error (name args)
  (cypher-signal "InvalidArgumentValue"
                 :detail (format nil "~a(~{~a~^, ~})" name
                                 (mapcar #'cypher-type-name args))))

(defun %to-string (v)
  (cond ((%tv-null v) "null")
        ((eq v t) "true")
        ((cypher-false-p v) "false")
        ((stringp v) v)
        ((floatp v) (format nil "~f" v))
        ((integerp v) (format nil "~d" v))
        (t (%fn-error "toString" (list v)))))

(defun %to-integer (v)
  (cond ((integerp v) v)
        ((floatp v) (values (truncate v)))
        ((stringp v)
         (let* ((s2 (string-trim '(#\Space #\Tab) v))
                (n (and (plusp (length s2))
                        (every (lambda (c) (or (digit-char-p c) (char= c #\-)))
                               (subseq s2 (if (and (plusp (length s2))
                                                   (char= (char s2 0) #\-))
                                              1 0)))
                        (handler-case (parse-integer s2 :junk-allowed nil)
                          (error () nil)))))
           (cond
             (n n)
             ((every (lambda (c)
                       (or (digit-char-p c) (member c '(#\. #\+ #\- #\e #\E))))
                     s2)
              (let ((f (%parse-float-sci s2)))
                (if f (values (truncate f)) :cypher-null)))
             (t :cypher-null))))
        ((or (eq v t) (cypher-false-p v)) (if (eq v t) 1 0))
        ((%tv-null v) :cypher-null)
        (t (%fn-error "toInteger" (list v)))))

(defun %parse-float-sci (s)
  "Parse a float string including scientific notation (1e3, 1.5E-3)."
  (let ((s2 (string-trim '(#\Space #\Tab) s)))
    (if (or (zerop (length s2))
            (not (every (lambda (c)
                          (or (digit-char-p c) (member c '(#\. #\+ #\- #\e #\E))))
                        s2)))
        nil
        (handler-case (values (read-from-string s2))
          (error () nil)))))

(defun %to-float (v)
  (cond ((floatp v) v)
        ((integerp v) (float v))
        ((stringp v) (or (handler-case (parse-float v :junk-allowed nil)
                           (error () nil))
                         (%parse-float-sci v)
                         :cypher-null))
        ((%tv-null v) :cypher-null)
        (t (%fn-error "toFloat" (list v)))))

(defun %to-boolean (v)
  (cond ((eq v t) t)
        ((cypher-false-p v) :cypher-false)
        ((stringp v) (cond ((string-equal v "true") t)
                           ((string-equal v "false") :cypher-false)
                           (t :cypher-null)))
        ((%tv-null v) :cypher-null)
        (t (%fn-error "toBoolean" (list v)))))

(defun %numeric (v)
  (and (numberp v) (not (%tv-null v))))

(defun %call-scalar (name args graph row params)
  (declare (ignore graph))
  (flet ((a0 () (first args))
         (a1 () (second args))
         (a2 () (third args)))
    (let ((arg0 (a0)) (arg1 (a1)))
      (cond
        ((string-equal name "coalesce")
         (dolist (x args :cypher-null)
           (unless (%tv-null x) (return x))))
        ((string-equal name "head")
         (cond ((%tv-null arg0) :cypher-null)
               ((cypher-empty-list-p arg0) :cypher-null)
               ((cypher-list-p arg0) (first (cypher-list-elements arg0)))
               (t (%fn-error name args))))
        ((string-equal name "last")
         (cond ((%tv-null arg0) :cypher-null)
               ((cypher-empty-list-p arg0) :cypher-null)
               ((cypher-list-p arg0) (car (last (cypher-list-elements arg0))))
               (t (%fn-error name args))))
        ((string-equal name "tail")
         (cond ((%tv-null arg0) :cypher-null)
               ((cypher-list-p arg0)
                (cypher-list (rest (cypher-list-elements arg0))))
               (t (%fn-error name args))))
        ((string-equal name "size")
         (cond ((%tv-null arg0) :cypher-null)
               ((stringp arg0) (length arg0))
               ((cypher-list-p arg0) (length (cypher-list-elements arg0)))
               (t (%fn-error name args))))
        ((string-equal name "length")
         (cond ((%tv-null arg0) :cypher-null)
               ((%path-p arg0) (/ (1- (length (second arg0))) 2))
               (t (%fn-error name args))))
        ((string-equal name "toBoolean") (%to-boolean arg0))
        ((string-equal name "toInteger") (%to-integer arg0))
        ((string-equal name "toFloat") (%to-float arg0))
        ((string-equal name "toString") (%to-string arg0))
        ((string-equal name "type")
         (cond ((%rel-p arg0) (%check-entity-live arg0 graph) (getf arg0 :type))
               ((%tv-null arg0) :cypher-null)
               (t (%fn-error name args))))
        ((string-equal name "labels")
         (cond ((%node-p arg0) (%check-entity-live arg0 graph)
                (cypher-list (getf arg0 :labels)))
               ((%tv-null arg0) :cypher-null)
               (t (%fn-error name args))))
        ((string-equal name "keys")
         (cond ((or (%node-p arg0) (%rel-p arg0) (cypher-map-p arg0))
                (cypher-list (mapcar #'car (cypher-map-pairs
                                            (if (cypher-map-p arg0)
                                                arg0
                                                (cypher-map (getf arg0 :props)))))))
               ((%tv-null arg0) :cypher-null)
               (t (cypher-signal "InvalidArgumentType"
                                  :detail (format nil "keys(~a)" (cypher-type-name arg0))))))
        ((string-equal name "properties")
         (cond ((or (%node-p arg0) (%rel-p arg0))
                (cypher-map (getf arg0 :props)))
               ((cypher-map-p arg0) arg0)
               ((%tv-null arg0) :cypher-null)
               (t (cypher-signal "InvalidArgumentType"
                                  :detail (format nil "properties(~a)"
                                                  (cypher-type-name arg0))))))
        ((string-equal name "id")
         (cond ((or (%node-p arg0) (%rel-p arg0))
                (logand (hash-string (getf arg0 :id)) #x7FFFFFFFFFFFFFFF))
               ((%tv-null arg0) :cypher-null)
               (t (%fn-error name args))))
        ((string-equal name "startNode")
         (cond ((%rel-p arg0) (getf arg0 :start-node))
               ((%tv-null arg0) :cypher-null)
               (t (%fn-error name args))))
        ((string-equal name "endNode")
         (cond ((%rel-p arg0) (getf arg0 :end-node))
               ((%tv-null arg0) :cypher-null)
               (t (%fn-error name args))))
        ((string-equal name "nodes")
         (cond ((%path-p arg0)
                (cypher-list (loop for (n r) on (second arg0) by #'cddr collect n)))
               ((%tv-null arg0) :cypher-null)
               (t (%fn-error name args))))
        ((string-equal name "relationships")
         (cond ((%path-p arg0)
                (cypher-list (loop for (n r) on (cdr (second arg0)) by #'cddr collect r)))
               ((%tv-null arg0) :cypher-null)
               (t (%fn-error name args))))
        ((string-equal name "abs")
         (if (%numeric arg0) (abs arg0) (if (%tv-null arg0) :cypher-null (%fn-error name args))))
        ((string-equal name "ceil")
         (if (%numeric arg0) (ceiling arg0) (if (%tv-null arg0) :cypher-null (%fn-error name args))))
        ((string-equal name "floor")
         (if (%numeric arg0) (floor arg0) (if (%tv-null arg0) :cypher-null (%fn-error name args))))
        ((string-equal name "round")
         (if (%numeric arg0) (round arg0) (if (%tv-null arg0) :cypher-null (%fn-error name args))))
        ((string-equal name "sign")
         (if (%numeric arg0) (if (minusp arg0) -1 (if (zerop arg0) 0 1))
             (if (%tv-null arg0) :cypher-null (%fn-error name args))))
        ((string-equal name "sqrt")
         (if (%numeric arg0) (sqrt (float arg0))
             (if (%tv-null arg0) :cypher-null (%fn-error name args))))
        ((string-equal name "toUpper")
         (if (stringp arg0) (string-upcase arg0)
             (if (%tv-null arg0) :cypher-null (%fn-error name args))))
        ((string-equal name "toLower")
         (if (stringp arg0) (string-downcase arg0)
             (if (%tv-null arg0) :cypher-null (%fn-error name args))))
        ((string-equal name "trim")
         (if (stringp arg0) (string-trim '(#\Space #\Tab #\Newline #\Return) arg0)
             (if (%tv-null arg0) :cypher-null (%fn-error name args))))
        ((string-equal name "lTrim")
         (if (stringp arg0) (string-left-trim '(#\Space #\Tab #\Newline #\Return) arg0)
             (if (%tv-null arg0) :cypher-null (%fn-error name args))))
        ((string-equal name "rTrim")
         (if (stringp arg0) (string-right-trim '(#\Space #\Tab #\Newline #\Return) arg0)
             (if (%tv-null arg0) :cypher-null (%fn-error name args))))
        ((string-equal name "reverse")
         (cond ((stringp arg0) (reverse arg0))
               ((cypher-list-p arg0)
                (cypher-list (reverse (cypher-list-elements arg0))))
               ((%tv-null arg0) :cypher-null)
               (t (%fn-error name args))))
        ((string-equal name "rand")
         (if (null args)
             (random 1.0d0)
             (%fn-error name args)))
        ((string-equal name "replace")
         (if (and (stringp arg0) (stringp (a1)) (stringp (a2)))
             (let ((out (make-string-output-stream)))
               (loop with i = 0 with n = (length arg0)
                     while (< i n)
                     do (let ((pos (search (a1) arg0 :start2 i)))
                          (if pos
                              (progn (write-string arg0 out :start i :end pos)
                                     (write-string (a2) out)
                                     (setf i (+ pos (length (a1)))))
                              (progn (write-string arg0 out :start i)
                                     (setf i n)))))
               (get-output-stream-string out))
             (if (or (%tv-null arg0) (%tv-null (a1)) (%tv-null (a2)))
                 :cypher-null
                 (%fn-error name args))))
        ((string-equal name "split")
         (if (and (stringp arg0) (stringp (a1)))
             (cypher-list
              (loop with start = 0 with n = (length arg0)
                    for pos = (search (a1) arg0 :start2 start)
                    collect (subseq arg0 start (or pos n)) into parts
                    while pos
                    do (setf start (+ pos (length (a1))))
                    finally (return parts)))
             (if (or (%tv-null arg0) (%tv-null (a1)))
                 :cypher-null
                 (%fn-error name args))))
        ((string-equal name "left")
         (if (and (stringp arg0) (integerp (a1)))
             (subseq arg0 0 (max 0 (min (a1) (length arg0))))
             (if (or (%tv-null arg0) (%tv-null (a1))) :cypher-null (%fn-error name args))))
        ((string-equal name "right")
         (if (and (stringp arg0) (integerp (a1)))
             (subseq arg0 (max 0 (- (length arg0) (a1))))
             (if (or (%tv-null arg0) (%tv-null (a1))) :cypher-null (%fn-error name args))))
        ((string-equal name "substring")
         (if (and (stringp arg0) (integerp (a1)))
             (let ((start (max 0 (a1)))
                   (end (if (and (a2) (integerp (a2)))
                            (min (length arg0) (+ (a1) (a2)))
                            (length arg0))))
               (if (or (>= start (length arg0)) (<= end start))
                   ""
                   (subseq arg0 start end)))
             (if (or (%tv-null arg0) (%tv-null (a1))) :cypher-null (%fn-error name args))))
        ((string-equal name "range")
         (cond
           ((or (%tv-null arg0) (%tv-null (a1)) (%tv-null (a2))) :cypher-null)
           ((not (and (integerp arg0) (integerp (a1))))
            (cypher-signal "InvalidArgumentType"
                           :detail "range: start and end must be integers"))
           ((and (a2) (not (integerp (a2))))
            (cypher-signal "InvalidArgumentType"
                           :detail "range: step must be an integer"))
           ((and (a2) (zerop (a2)))
            (cypher-signal "NumberOutOfRange"
                           :detail "range: step must not be zero"))
           (t (let ((step (if (a2) (a2) 1)))
                (cypher-list
                 (loop for x = arg0 then (+ x step)
                       while (if (plusp step) (<= x (a1)) (>= x (a1)))
                       collect x))))))
        ((string-equal name "exists")
         (%fn-error name args))
        (t (cypher-signal "InvalidArgumentType"
                          :detail (format nil "unknown function ~a" name)))))))

;;; ------------------------------------------------------------------
;;; the expression evaluator

(defun %aggregate-fn-p (name)
  (member name '("count" "sum" "avg" "min" "max" "collect") :test #'string-equal))

(defun expr-has-aggregate (expr)
  "True when EXPR contains an aggregate call (or count(*))."
  (cond
    ((atom expr) nil)
    ((eq (car expr) :count-*) t)
    ((eq (car expr) :call)
     (if (%aggregate-fn-p (getf (cdr expr) :fn))
         t
         (some #'expr-has-aggregate (getf (cdr expr) :args))))
    (t (some #'expr-has-aggregate (cdr expr)))))

(defun %coerce-number (v float?)
  (if float?
      (if (integerp v) (float v) v)
      v))

#+sbcl
(defun %nan ()
  "A quiet NaN double-float (SBCL: 0.0/0.0 with traps masked)."
  (sb-int:with-float-traps-masked (:divide-by-zero :invalid)
    (/ 0.0d0 0.0d0)))
#-sbcl
(defun %nan () 0.0d0)

#+sbcl
(defun %float-inf (x)
  "Positive/negative infinity of the same float type as X."
  (sb-int:with-float-traps-masked (:divide-by-zero)
    (/ (coerce 1.0 (type-of x)) 0.0)))
#-sbcl
(defun %float-inf (x) (coerce 1.0 (type-of x)))

(defun %arith (op a b)
  (cond
    ((or (%tv-null a) (%tv-null b)) :cypher-null)
    ((and (eq op :+) (cypher-list-p a) (cypher-list-p b))
     (cypher-list (append (cypher-list-elements a) (cypher-list-elements b))))
    ((and (eq op :+) (cypher-list-p a))
     ;; list + element appends the element
     (cypher-list (append (cypher-list-elements a) (list b))))
    ((and (eq op :+) (cypher-list-p b))
     ;; element + list prepends the element
     (cypher-list (cons a (cypher-list-elements b))))
    ((and (eq op :+) (cypher-list-p a) (%tv-null b)) a)
    ((and (eq op :+) (%tv-null a) (cypher-list-p b)) b)
    ((and (eq op :+) (numberp a) (stringp b))
     (concatenate 'string (format nil "~a" a) b))
    ((stringp a)
     (if (and (stringp b) (eq op :+))
         (concatenate 'string a b)
         (if (and (eq op :+) (numberp b))
             (concatenate 'string a (format nil "~a" b))
             (cypher-signal "InvalidArgumentType"
                            :detail (format nil "~a(~a, ~a)" op
                                            (cypher-type-name a)
                                            (cypher-type-name b))))))
    ((and (numberp a) (numberp b))
     (let ((float? (or (floatp a) (floatp b))))
       (case op
         (:+ (+ a b))
         (:- (- a b))
         (:* (* a b))
         (:/ (cond
               ((zerop b)
                (cond ((floatp a)
                       ;; IEEE-754 float division by zero: 0.0/0.0 -> NaN,
                       ;; x/0.0 -> infinity (integer division stays null)
                       (if (zerop a)
                           (scalaxy::%nan)
                           (scalaxy::%float-inf a)))
                      (t :cypher-null)))
               ((and (integerp a) (integerp b)) (truncate a b))
               (t (/ (%coerce-number a t) (%coerce-number b t)))))
         (:% (if (zerop b) :cypher-null (mod a b)))
         (:^ (expt (%coerce-number a float?) (%coerce-number b float?))))))
    (t (cypher-signal "InvalidArgumentType"
                      :detail (format nil "~a(~a, ~a)" op
                                      (cypher-type-name a)
                                      (cypher-type-name b))))))

(defun %slice-list (v start end)
  "openCypher list/string slice V[START..END] with implicit bounds as NIL.
START is inclusive, END exclusive; negative bounds count from the end;
out-of-range bounds clamp; a null or non-integer bound yields :cypher-null."
  (let ((len (if (cypher-list-p v) (length (cypher-list-elements v)) (length v))))
    (flet ((resolve (b implicit)
             (cond ((null b) implicit)
                   ((%tv-null b) :cypher-null)
                   ((integerp b) b)
                   (t :cypher-null))))
      (let ((from (resolve start 0))
            (to (resolve end len)))
        (when (or (%tv-null from) (%tv-null to)) (return-from %slice-list :cypher-null))
        (let ((from (if (minusp from) (max 0 (+ len from)) (min from len)))
              (to (if (minusp to) (max 0 (+ len to)) (min to len))))
          (if (>= from to)
              (if (cypher-list-p v) (cypher-list nil) "")
              (if (cypher-list-p v)
                  (cypher-list (subseq (cypher-list-elements v) from to))
                  (subseq v from to))))))))

(defun %list-index (v i)
  (cond
    ((%tv-null v) :cypher-null)
    ((cypher-list-p v)
     (cond
       ((not (integerp i))
        (cypher-signal "InvalidArgumentType"
                       :detail (format nil "indexing a list with ~a" (cypher-type-name i))
                       :family "TypeError"))
       ((and (>= i 0) (< i (length (cypher-list-elements v))))
        (nth i (cypher-list-elements v)))
       (t :cypher-null)))
    ((and (stringp v) (integerp i))
     (if (and (>= i 0) (< i (length v)))
         (string (char v i))
         :cypher-null))
    ((stringp v)
     (cypher-signal "InvalidArgumentType"
                    :detail (format nil "indexing a string with ~a" (cypher-type-name i))
                    :family "TypeError"))
    ((cypher-map-p v)
     (cypher-signal "MapElementAccessByNonString"
                    :detail (format nil "indexing a map with ~a" (cypher-type-name i))))
    (t (cypher-signal "InvalidArgumentType"
                      :detail (format nil "indexing a ~a" (cypher-type-name v))
                      :family "TypeError"))))

(defun %map-access (m k)
  (cond
    ((%tv-null m) :cypher-null)
    ((or (cypher-map-p m) (%node-p m) (%rel-p m))
     (if (stringp k)
         (let ((p (assoc k (if (cypher-map-p m)
                               (cypher-map-pairs m)
                               (getf m :props))
                         :test #'equal)))
           (if p (cdr p) :cypher-null))
         (cypher-signal "MapElementAccessByNonString"
                        :detail (format nil "property key ~a" k))))
    (t (cypher-signal "InvalidArgumentType"
                      :detail (format nil "indexing a ~a" (cypher-type-name m))))))

(defun %expr-identical-p (a b)
  "True when the two AST operands are structurally identical complex
expressions (not trivial literals/variables/params).  The reference
treats X = X as true for identical complex expressions."
  (and (consp a) (consp b)
       (not (member (car a) '(:lit :var :param :map :list)))
       ;; division/modulo/power can produce NaN, and NaN /= NaN
       (not (and (eq (car a) :bin)
                 (member (second a) '(:/ :% :^))))
       (equal a b)))

(defun %check-entity-live (v graph)
  "Signal DeletedEntityAccess when V is a node/relationship that no
longer exists in GRAPH (openCypher: deleted entities are inaccessible)."
  (when graph
    (when (and (%node-p v) (null (graph-node graph (getf v :id))))
      (cypher-signal "DeletedEntityAccess" :detail "node has been deleted"))
    (when (and (%rel-p v) (null (graph-relationship graph (getf v :id))))
      (cypher-signal "DeletedEntityAccess" :detail "relationship has been deleted")))
  v)

(defun %eval-bin (op a b &optional ast-a ast-b)
  (case op
    ((:and) (%tv-and a b))
    ((:or) (%tv-or a b))
    ((:xor) (%tv-xor a b))
    ((:=) (if (%expr-identical-p ast-a ast-b)
              t
              (cypher-= a b)))
    ((:<>) (if (%expr-identical-p ast-a ast-b)
               :cypher-false
               (let ((eq? (cypher-= a b)))
                 (cond
                   ((cypher-null-p eq?) :cypher-null)
                   ((%tv-true eq?) :cypher-false)
                   (t t)))))
    ((:< :> :<= :>=)
     (let ((c (cypher-compare a b)))
       (case c
         (:null :cypher-null)
         (:eq (if (member op '(:<= :>=)) t :cypher-false))
         (:lt (if (member op '(:< :<=)) t :cypher-false))
         (:gt (if (member op '(:> :>=)) t :cypher-false)))))
    ((:+ :- :* :/ :% :^) (%arith op a b))
    ((:in) (%in-op a b))
    ((:starts) (%string-op a b :starts))
    ((:ends) (%string-op a b :ends))
    ((:contains) (%string-op a b :contains))
    ((:=~) (cypher-signal "InvalidArgumentType"
                          :detail "regular expressions are not supported"))
    (t (cypher-signal "InvalidArgumentType" :detail (format nil "unknown operator ~a" op)))))

(defun %in-op (a b)
  (cond
    ((%tv-null b) :cypher-null)
    ((cypher-list-p b)
     (let ((found-null? nil))
       (dolist (x (cypher-list-elements b))
         (let ((eq? (cypher-= a x)))
           (cond ((%tv-true eq?) (return-from %in-op t))
                 ((%tv-null eq?) (setf found-null? t)))))
       (if found-null? :cypher-null :cypher-false)))
    ((%tv-null a) :cypher-null)
    (t (cypher-signal "InvalidArgumentType"
                      :detail (format nil "IN on non-list ~a" (cypher-type-name b))))))

(defun %string-op (a b which)
  (cond
    ((or (%tv-null a) (%tv-null b)) :cypher-null)
    ((and (stringp a) (stringp b))
     (if (zerop (length b))
         t
         (let ((pos (search b a)))
           (case which
             (:starts (if (and pos (zerop pos)) t :cypher-false))
             (:ends (if (and pos (= (+ pos (length b)) (length a))) t :cypher-false))
             (:contains (if pos t :cypher-false))))))
    ;; a string predicate with a non-string (non-null) operand is null,
    ;; not an error (openCypher string predicates)
    (t :cypher-null)))

(defun %eval-case (form row graph params)
  (let ((base-expr (getf (cdr form) :base)))
    (let ((result
            (if base-expr
                (let ((base (eval-expr base-expr row graph params)))
                  (dolist (clause (getf (cdr form) :clauses) :cypher-null)
                    (let ((w (eval-expr (car clause) row graph params)))
                      (when (%tv-true (cypher-= base w))
                        (return (eval-expr (cdr clause) row graph params))))))
                (dolist (clause (getf (cdr form) :clauses) :cypher-null)
                  (when (%tv-true (eval-expr (car clause) row graph params))
                    (return (eval-expr (cdr clause) row graph params)))))))
      (if (%tv-null result)
          (if (getf (cdr form) :else)
              (eval-expr (getf (cdr form) :else) row graph params)
              :cypher-null)
          result))))

(defun %eval-pred (form row graph params matcher)
  (let* ((kind (getf (cdr form) :kind))
         (var (getf (cdr form) :var))
         (list-v (eval-expr (getf (cdr form) :list) row graph params))
         (items (if (cypher-list-p list-v) (cypher-list-elements list-v) nil)))
    (if (not (cypher-list-p list-v))
        ;; quantifier over a non-list (including null) is null
        :cypher-null
        (let ((trues 0) (nulls 0) (falses 0))
          (dolist (x items)
            (let ((v (eval-expr (getf (cdr form) :pred) (acons var x row) graph params)))
              (cond ((%tv-true v) (incf trues))
                    ((%tv-null v) (incf nulls))
                    (t (incf falses)))))
          (case kind
            (:all (cond ((plusp falses) :cypher-false)
                        ((plusp nulls) :cypher-null)
                        (t t)))
            (:any (cond ((plusp trues) t)
                        ((plusp nulls) :cypher-null)
                        (t :cypher-false)))
            (:none (cond ((plusp trues) :cypher-false)
                         ((plusp nulls) :cypher-null)
                         (t t)))
            (:single (cond ((> trues 1) :cypher-false)
                           ((= trues 1) (if (plusp nulls) :cypher-null t))
                           (t (if (plusp nulls) :cypher-null :cypher-false)))))))))

(defun eval-expr (expr row graph params)
  "Evaluate EXPR against ROW (alist of var -> value).  GRAPH is the
graph-view used by EXISTS patterns; PARAMS is a hash-table or nil."
  (cond
    ((symbolp expr)
     (let ((p (assoc expr row)))
       (unless p
         (cypher-signal "UndefinedVariable" :detail (symbol-name expr)))
       (cdr p)))
    ((atom expr)
     (cypher-signal "InvalidArgumentType" :detail (format nil "bad expression ~s" expr)))
    (t
     (ecase (car expr)
       (:lit (second expr))
       (:param (let ((name (second expr)))
                 (if params
                     (multiple-value-bind (v found?) (gethash name params)
                       (if found? v
                           (cypher-signal "InvalidParameterUse"
                                          :detail (format nil "missing parameter $~a" name))))
                     (cypher-signal "InvalidParameterUse"
                                    :detail (format nil "missing parameter $~a" name)))))
       (:var (let ((p (assoc (second expr) row)))
               (unless p
                 (cypher-signal "UndefinedVariable" :detail (symbol-name (second expr))))
               (cdr p)))
       (:prop (let ((v (eval-expr (getf (cdr expr) :expr) row graph params)))
                (cond ((%tv-null v) :cypher-null)
                      ((or (%node-p v) (%rel-p v) (cypher-map-p v))
                       (%check-entity-live v graph)
                       (%map-access v (getf (cdr expr) :prop)))
                      (t (cypher-signal "InvalidArgumentType"
                                         :detail (format nil "property access on ~a"
                                                         (cypher-type-name v)))))))
       (:idx (let ((v (eval-expr (getf (cdr expr) :expr) row graph params))
                   (i (eval-expr (getf (cdr expr) :index) row graph params)))
               (cond
                 ((and (stringp i)
                       (or (cypher-map-p v) (%node-p v) (%rel-p v)))
                  (%check-entity-live v graph)
                  (%map-access v i))
                 ((%tv-null v) :cypher-null)
                 (t (%list-index v i)))))
       (:has-labels (let ((v (eval-expr (getf (cdr expr) :expr) row graph params))
                          (labels (getf (cdr expr) :labels)))
                       (cond
                         ((%tv-null v) :cypher-null)
                         ((%node-p v)
                          (if (every (lambda (l) (member l (getf v :labels)
                                                          :test #'string=))
                                     labels)
                              t :cypher-false))
                         ((%rel-p v)
                          ;; a relationship has exactly one type; a
                          ;; conjunction of types cannot match
                          (if (and (= (length labels) 1)
                                   (string= (first labels) (getf v :type)))
                              t :cypher-false))
                         (t (cypher-signal "InvalidArgumentType"
                                           :detail (format nil "label expression on ~a"
                                                           (cypher-type-name v)))))))
       (:slice (let ((v (eval-expr (getf (cdr expr) :expr) row graph params))
                     (start (when (getf (cdr expr) :start)
                              (eval-expr (getf (cdr expr) :start) row graph params)))
                     (end (when (getf (cdr expr) :end)
                            (eval-expr (getf (cdr expr) :end) row graph params))))
                 (cond
                   ((%tv-null v) :cypher-null)
                   ((or (cypher-list-p v) (stringp v)) (%slice-list v start end))
                   (t (cypher-signal "InvalidArgumentType"
                                     :detail (format nil "slicing a ~a"
                                                     (cypher-type-name v)))))))
       (:bin (%eval-bin (second expr)
                        (eval-expr (third expr) row graph params)
                        (eval-expr (fourth expr) row graph params)
                        (third expr) (fourth expr)))
       (:not (%tv-not (eval-expr (second expr) row graph params)))
       (:neg (let ((v (eval-expr (second expr) row graph params)))
               (if (%tv-null v) :cypher-null
                   (if (numberp v) (- v) (%fn-error "-" (list v))))))
       (:is-null (if (%tv-null (eval-expr (getf (cdr expr) :expr) row graph params))
                     t :cypher-false))
       (:is-not-null (if (%tv-null (eval-expr (getf (cdr expr) :expr) row graph params))
                         :cypher-false t))
       (:call (if (and *agg-finish-hook*
                        (%aggregate-fn-p (getf (cdr expr) :fn)))
                  (funcall *agg-finish-hook* expr)
                  (%call-scalar (getf (cdr expr) :fn)
                                (mapcar (lambda (a) (eval-expr a row graph params))
                                        (getf (cdr expr) :args))
                                graph row params)))
       (:count-* (if *agg-finish-hook*
                     (funcall *agg-finish-hook* expr)
                     (cypher-signal "InvalidAggregation"
                                    :detail "count(*) outside of aggregation")))
       (:list (cypher-list (mapcar (lambda (a) (eval-expr a row graph params))
                                   (getf (cdr expr) :items))))
       (:map (cypher-map (mapcar (lambda (p)
                                   (cons (car p) (eval-expr (cdr p) row graph params)))
                                 (getf (cdr expr) :pairs))))
       (:pcomp (if *pcomp-matcher*
                   (funcall *pcomp-matcher* row expr)
                   (cypher-signal "InvalidArgumentType"
                                  :detail "pattern comprehension unavailable")))
       (:comp (let ((v (eval-expr (getf (cdr expr) :list) row graph params)))
                (if (cypher-list-p v)
                    (let ((out nil))
                      (dolist (x (cypher-list-elements v))
                        (let ((r2 (row-bind row (getf (cdr expr) :var) x)))
                          (when (or (null (getf (cdr expr) :where))
                                    (%tv-true (eval-expr (getf (cdr expr) :where)
                                                         r2 graph params)))
                            (push (if (getf (cdr expr) :out)
                                      (eval-expr (getf (cdr expr) :out) r2 graph params)
                                      x)
                                  out))))
                      (cypher-list (nreverse out)))
                    (cypher-signal "InvalidArgumentType"
                                   :detail "list comprehension over non-list"))))
       (:case (%eval-case expr row graph params))
       (:pred (%eval-pred expr row graph params nil))
       (:exists (if *exists-matcher*
                    (if (funcall *exists-matcher* row expr) t :cypher-false)
                    (cypher-signal "InvalidArgumentType"
                                   :detail "exists() unavailable")))
       (:exists-sub (if *exists-matcher*
                        (if (funcall *exists-matcher* row expr) t :cypher-false)
                        (cypher-signal "InvalidArgumentType"
                                       :detail "exists() unavailable")))))))

(defvar *exists-matcher* nil
  "Function (ROW PATTERN) -> boolean, bound by the executor.")

(defvar *pcomp-matcher* nil
  "Function (ROW PCOMP) -> list of outputs, bound by the executor.")

(defvar *agg-finish-hook* nil
  "Function (AGG-EXPR) -> finished aggregate value; bound by the
executor when evaluating expressions over grouped rows.")
