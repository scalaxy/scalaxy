;;;; lexer.lisp --- hand-written Cypher tokenizer (dependency-free)
;;;;
;;;; Pure function: (CYPHER-LEX string) -> list of tokens, or signals a
;;;; Cypher syntax error.  Numbers are parsed here (decimal, hex 0x,
;;;; octal 0..., floats with exponents); the spec's InvalidNumberLiteral
;;;; and IntegerOverflow errors are raised at this stage.

(in-package #:scalaxy)

(defstruct (cytoken (:constructor make-cytoken (kind value line col pos end)))
  kind    ; :eof :ident :string :int :float :param :punct
  value   ; string | integer | float | operator string
  line
  col
  pos     ; char offset of the first char of the token in the source
  end)    ; char offset just past the last char of the token

(defstruct (lexer-ctx (:constructor make-lexer-ctx (string query)))
  string
  query
  (start 0)
  (i 0)
  (n 0)
  (line 1)
  (col 1)
  tokens)

(defun %ident-char-p (ch)
  (and ch (or (alphanumericp ch) (char= ch #\_))))

(defun %digit-p (ch) (and ch (digit-char-p ch)))

(defun %hex-p (ch) (and ch (digit-char-p ch 16)))

(defun %oct-p (ch) (and ch (char<= #\0 ch #\7)))

(defun lx-advance (lx)
  (let ((s (lexer-ctx-string lx)))
    (when (< (lexer-ctx-i lx) (lexer-ctx-n lx))
      (if (char= (char s (lexer-ctx-i lx)) #\Newline)
          (progn (incf (lexer-ctx-line lx)) (setf (lexer-ctx-col lx) 1))
          (incf (lexer-ctx-col lx)))
      (incf (lexer-ctx-i lx)))))

(defun lx-peek (lx)
  (and (< (lexer-ctx-i lx) (lexer-ctx-n lx))
       (char (lexer-ctx-string lx) (lexer-ctx-i lx))))

(defun lx-peek2 (lx)
  (and (< (1+ (lexer-ctx-i lx)) (lexer-ctx-n lx))
       (char (lexer-ctx-string lx) (1+ (lexer-ctx-i lx)))))

(defun lx-err (lx kind detail)
  (cypher-signal kind
                 :query (lexer-ctx-query lx)
                 :detail (format nil "~a at line ~d, column ~d"
                                 detail (lexer-ctx-line lx) (lexer-ctx-col lx))))

(defun lx-emit (lx kind value)
  (push (make-cytoken kind value (lexer-ctx-line lx) (lexer-ctx-col lx)
                      (lexer-ctx-start lx) (lexer-ctx-i lx))
        (lexer-ctx-tokens lx)))

(defun lx-skip-ws (lx)
  (loop while (and (< (lexer-ctx-i lx) (lexer-ctx-n lx))
                   (member (lx-peek lx) '(#\Space #\Tab #\Newline #\Return)))
        do (lx-advance lx)))

(defun lx-skip-line-comment (lx)
  (lx-advance lx) (lx-advance lx)
  (loop while (and (< (lexer-ctx-i lx) (lexer-ctx-n lx))
                   (not (char= (lx-peek lx) #\Newline)))
        do (lx-advance lx)))

(defun lx-skip-block-comment (lx)
  (lx-advance lx) (lx-advance lx)
  (loop until (or (>= (lexer-ctx-i lx) (lexer-ctx-n lx))
                  (and (char= (lx-peek lx) #\*)
                       (eql (lx-peek2 lx) #\/)))
        do (lx-advance lx))
  (when (< (lexer-ctx-i lx) (lexer-ctx-n lx))
    (lx-advance lx) (lx-advance lx)))

(defun lx-unicode-escape (lx out)
  (let ((code 0))
    (dotimes (k 4)
      (when (>= (lexer-ctx-i lx) (lexer-ctx-n lx))
        (lx-err lx "InvalidUnicodeLiteral" "truncated unicode escape"))
      (let ((h (lx-peek lx)))
        (lx-advance lx)
        (unless (%hex-p h)
          (lx-err lx "InvalidUnicodeLiteral" "invalid unicode escape"))
        (setf code (+ (* code 16) (digit-char-p h 16)))))
    (write-char (code-char code) out)))

(defun lx-escape (lx out)
  (lx-advance lx)
  (when (>= (lexer-ctx-i lx) (lexer-ctx-n lx))
    (lx-err lx "UnexpectedSyntax" "unterminated escape"))
  (let ((esc (lx-peek lx)))
    (lx-advance lx)
    (case esc
      (#\\ (write-char #\\ out))
      (#\' (write-char #\' out))
      (#\" (write-char #\" out))
      (#\n (write-char #\Newline out))
      (#\t (write-char #\Tab out))
      (#\r (write-char #\Return out))
      (#\b (write-char #\Backspace out))
      (#\f (write-char #\Page out))
      (#\u (lx-unicode-escape lx out))
      (t (lx-err lx "UnexpectedSyntax"
                 (format nil "unknown escape \\~c" esc))))))

(defun lx-string (lx quote-char)
  (lx-advance lx)
  (let ((result
          (with-output-to-string (out)
            (loop
              (when (>= (lexer-ctx-i lx) (lexer-ctx-n lx))
                (lx-err lx "UnexpectedSyntax" "unterminated string literal"))
              (let ((ch (lx-peek lx)))
                (when (char= ch quote-char)
                  (lx-advance lx)
                  (return))
                (if (char= ch #\\)
                    (lx-escape lx out)
                    (progn (write-char ch out) (lx-advance lx))))))))
    (lx-emit lx :string result)))

(defun %int64-overflow-p (v prev-char)
  "True when V is outside the int64 range, except the exact
min-int64 magnitude 9223372036854775808 which is legal after a minus
sign (-9223372036854775808 is the smallest int64)."
  (or (> v 9223372036854775808)
      (and (= v 9223372036854775808)
           (not (and prev-char (char= prev-char #\-))))))

(defun lx-number (lx)
  (let* ((s (lexer-ctx-string lx))
         (start (lexer-ctx-i lx)))
    (cond
      ((and (char= (lx-peek lx) #\0) (eql (lx-peek2 lx) #\x))
       (lx-advance lx) (lx-advance lx)
       (let ((digits 0))
         (loop while (%hex-p (lx-peek lx))
               do (incf digits) (lx-advance lx))
         (when (zerop digits)
           (lx-err lx "InvalidNumberLiteral" "hex literal needs digits"))
         (when (%ident-char-p (lx-peek lx))
           (lx-err lx "InvalidNumberLiteral" "junk after hex literal"))
         (let ((v (parse-integer s :start (+ start 2)
                                 :end (lexer-ctx-i lx) :radix 16)))
           (when (%int64-overflow-p v (and (plusp start) (char s (1- start))))
             (lx-err lx "IntegerOverflow" (subseq s start (lexer-ctx-i lx))))
           (lx-emit lx :int v))))
      ((and (char= (lx-peek lx) #\0) (eql (lx-peek2 lx) #\o))
       ;; modern 0oNNN octal literal (openCypher Literals4)
       (lx-advance lx) (lx-advance lx)
       (let ((digits 0))
         (loop while (%oct-p (lx-peek lx))
               do (incf digits) (lx-advance lx))
         (when (zerop digits)
           (lx-err lx "InvalidNumberLiteral" "octal literal needs digits"))
         (when (%ident-char-p (lx-peek lx))
           (lx-err lx "InvalidNumberLiteral" "junk after octal literal"))
         (let ((v (parse-integer s :start (+ start 2)
                                 :end (lexer-ctx-i lx) :radix 8)))
           (when (%int64-overflow-p v (and (plusp start) (char s (1- start))))
             (lx-err lx "IntegerOverflow" (subseq s start (lexer-ctx-i lx))))
           (lx-emit lx :int v))))
      ((and (char= (lx-peek lx) #\0) (%digit-p (lx-peek2 lx)))
       (lx-advance lx)
       (loop while (and (%digit-p (lx-peek lx))
                        (if (%oct-p (lx-peek lx))
                            t
                            (progn (lx-err lx "InvalidNumberLiteral" "invalid octal digit")
                                   nil)))
             do (lx-advance lx))
       (let ((v (parse-integer s :start start
                               :end (lexer-ctx-i lx) :radix 8)))
         (when (%int64-overflow-p v (and (plusp start) (char s (1- start))))
           (lx-err lx "IntegerOverflow" (subseq s start (lexer-ctx-i lx))))
         (lx-emit lx :int v)))
      (t
       (loop while (%digit-p (lx-peek lx)) do (lx-advance lx))
       (let ((float? nil))
         (when (and (lx-peek lx)
                    (char= (lx-peek lx) #\.)
                    (%digit-p (lx-peek2 lx)))
           (setf float? t)
           (lx-advance lx)
           (loop while (%digit-p (lx-peek lx)) do (lx-advance lx)))
         (when (and (member (lx-peek lx) '(#\e #\E))
                    (or (%digit-p (lx-peek2 lx))
                        (and (member (lx-peek2 lx) '(#\+ #\-))
                             (and (< (+ (lexer-ctx-i lx) 2) (lexer-ctx-n lx))
                                  (%digit-p (char s (+ (lexer-ctx-i lx) 2)))))))
           (setf float? t)
           (lx-advance lx)
           (when (member (lx-peek lx) '(#\+ #\-)) (lx-advance lx))
           (loop while (%digit-p (lx-peek lx)) do (lx-advance lx)))
         (when (%ident-char-p (lx-peek lx))
           (lx-err lx "InvalidNumberLiteral" "junk after number"))
         (let ((text (subseq s start (lexer-ctx-i lx))))
           (if float?
               (let ((v
                       ;; the default read format is single-float, which
                       ;; overflows for values above ~3.4e38; retry as
                       ;; double-float before declaring overflow
                       (handler-case
                           (let ((*read-default-float-format* 'double-float))
                             (read-from-string text))
                         (error (e)
                           (if (search "FLOATING-POINT-OVERFLOW"
                                       (princ-to-string e))
                               (lx-err lx "FloatingPointOverflow" text)
                               (lx-err lx "InvalidNumberLiteral" text))))))
                 (when (and (floatp v) (not (<= (abs v) most-positive-double-float)))
                   (lx-err lx "FloatingPointOverflow" text))
                 (lx-emit lx :float v))
               (let ((v (handler-case (parse-integer text :radix 10)
                          (error () (lx-err lx "InvalidNumberLiteral" text)))))
                 (when (%int64-overflow-p v (and (plusp start) (char s (1- start))))
                   (lx-err lx "IntegerOverflow" text))
                 (lx-emit lx :int v)))))))))

(defun lx-param (lx)
  (lx-advance lx)
  (let ((start (lexer-ctx-i lx)))
    (loop while (%ident-char-p (lx-peek lx)) do (lx-advance lx))
    (when (= (lexer-ctx-i lx) start)
      (lx-err lx "InvalidParameterUse" "parameter needs a name"))
    (lx-emit lx :param (subseq (lexer-ctx-string lx) start (lexer-ctx-i lx)))))

(defun cypher-lex (string &key (query string))
  "Tokenize a Cypher query.  Signals Cypher syntax errors on malformed
input (InvalidNumberLiteral, IntegerOverflow, UnexpectedSyntax)."
  (let ((lx (make-lexer-ctx string query)))
    (setf (lexer-ctx-n lx) (length string))
    (loop
      (lx-skip-ws lx)
      (when (>= (lexer-ctx-i lx) (lexer-ctx-n lx))
        (lx-emit lx :eof nil)
        (return))
      (setf (lexer-ctx-start lx) (lexer-ctx-i lx))
      (let ((ch (lx-peek lx)))
        (cond
          ((and (char= ch #\/) (member (lx-peek2 lx) '(#\/ #\*)))
           (if (eql (lx-peek2 lx) #\/)
               (lx-skip-line-comment lx)
               (lx-skip-block-comment lx)))
          ((or (char= ch #\') (char= ch #\"))
           (lx-string lx ch))
          ((%digit-p ch) (lx-number lx))
          ((char= ch #\.)
           (cond
             ((%digit-p (lx-peek2 lx)) (lx-number lx))
             ((char= (lx-peek2 lx) #\.)
              (lx-advance lx) (lx-advance lx)
              (lx-emit lx :punct ".."))
             (t (lx-advance lx) (lx-emit lx :punct "."))))
          ((char= ch #\$) (lx-param lx))
          ((char= ch #\`)
           ;; backtick-quoted identifier: `name`, `` (empty), ``a``b`` (escaped)
           (lx-advance lx)
           (let ((out (make-string-output-stream)))
             (loop
               (let ((c (lx-peek lx)))
                 (cond
                   ((null c) (lx-err lx "UnexpectedSyntax" "unterminated backtick identifier"))
                   ((char= c #\`)
                    (lx-advance lx)
                    (if (and (lx-peek lx) (char= (lx-peek lx) #\`))
                        (progn (lx-advance lx) (write-char #\` out))
                        (return)))
                   (t (write-char c out) (lx-advance lx)))))
             (lx-emit lx :ident (get-output-stream-string out))))
          ((%ident-char-p ch)
           (let ((start (lexer-ctx-i lx)))
             (loop while (%ident-char-p (lx-peek lx)) do (lx-advance lx))
             (lx-emit lx :ident
                      (subseq (lexer-ctx-string lx) start (lexer-ctx-i lx)))))
          (t
           (let ((three (and (< (+ (lexer-ctx-i lx) 2) (lexer-ctx-n lx))
                             (subseq (lexer-ctx-string lx) (lexer-ctx-i lx)
                                     (min (lexer-ctx-n lx) (+ (lexer-ctx-i lx) 3)))))
                 (two (and (< (1+ (lexer-ctx-i lx)) (lexer-ctx-n lx))
                           (subseq (lexer-ctx-string lx) (lexer-ctx-i lx)
                                   (min (lexer-ctx-n lx) (+ (lexer-ctx-i lx) 2)))))
                 (four (and (< (+ 3 (lexer-ctx-i lx)) (lexer-ctx-n lx))
                            (subseq (lexer-ctx-string lx) (lexer-ctx-i lx)
                                    (min (lexer-ctx-n lx) (+ (lexer-ctx-i lx) 4))))))
             (cond
               ((member four '("<-->") :test #'string=)
                (lx-advance lx) (lx-advance lx) (lx-advance lx) (lx-advance lx)
                (lx-emit lx :punct four))
               ((member three '("-->" "<--" "<-[") :test #'string=)
                (lx-advance lx) (lx-advance lx) (lx-advance lx)
                (lx-emit lx :punct three))
               ((member two '("-[" "<-[" "->" "<-" "--" "<>" "<=" ">=" "+=" "=~" "..")
                        :test #'string=)
                (lx-advance lx) (lx-advance lx)
                (lx-emit lx :punct two))
               ((member ch '(#\( #\) #\[ #\] #\{ #\} #\, #\: #\; #\= #\< #\>
                            #\+ #\- #\* #\/ #\% #\^ #\|) :test #'char=)
                (lx-advance lx)
                (lx-emit lx :punct (string ch)))
               (t (if (and (char>= ch (code-char #x2000))
                           (char<= ch (code-char #x206F)))
                      ;; unicode punctuation/dashes are not valid Cypher
                      (lx-err lx "InvalidUnicodeCharacter"
                              (format nil "invalid unicode character ~c" ch))
                      (lx-err lx "UnexpectedSyntax"
                              (format nil "unexpected character ~c" ch))))))))))
    (nreverse (lexer-ctx-tokens lx))))
