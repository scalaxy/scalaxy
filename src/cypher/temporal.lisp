;;;; temporal.lisp --- openCypher temporal value types
;;;;
;;;; Implements date / time / localtime / datetime / localdatetime /
;;;; duration values (the openCypher temporal functions).  Values are
;;;; marker-prefixed lists:
;;;;   (:cypher-date        :year :month :day)
;;;;   (:cypher-localtime   :hour :minute :second :nanosecond)
;;;;   (:cypher-time        :hour :minute :second :nanosecond :offset)
;;;;   (:cypher-localdatetime :year :month :day :hour :minute :second :nanosecond)
;;;;   (:cypher-datetime    :year :month :day :hour :minute :second :nanosecond :offset)
;;;;   (:cypher-duration    :months :days :nanos)
;;;; OFFSET is in minutes east of UTC (16:00Z has offset 0).
;;;; NANOSECOND is 0..999999999; DURATION :nanos is signed.

(in-package #:scalaxy)

;;; ------------------------------------------------------------------
;;; value predicates

(defun %date-p (v)        (and (consp v) (eq (car v) :cypher-date)))
(defun %localtime-p (v)   (and (consp v) (eq (car v) :cypher-localtime)))
(defun %time-p (v)        (and (consp v) (eq (car v) :cypher-time)))
(defun %localdatetime-p (v) (and (consp v) (eq (car v) :cypher-localdatetime)))
(defun %datetime-p (v)    (and (consp v) (eq (car v) :cypher-datetime)))
(defun %duration-p (v)    (and (consp v) (eq (car v) :cypher-duration)))
(defun %temporal-p (v)
  (or (%date-p v) (%localtime-p v) (%time-p v)
      (%localdatetime-p v) (%datetime-p v) (%duration-p v)))

;;; ------------------------------------------------------------------
;;; civil calendar (proleptic Gregorian, arbitrary years)
;;; Port of Howard Hinnant's days<->civil algorithms.

(defun %civil-to-days (y m d)
  "Days since 1970-01-01 for the Gregorian date Y-M-D."
  (let* ((y (if (<= m 2) (1- y) y))
         (era (floor y 400))
         (yoe (- y (* era 400)))
         (doy (+ (floor (+ (* 153 (if (> m 2) (- m 3) (+ m 9))) 2) 5) d -1))
         (doe (+ (floor yoe 4) (- (floor yoe 100)) doy)))
    (+ (* era 146097) doe (* yoe 365) -719468)))

(defun %days-to-civil (z)
  "Z = days since 1970-01-01.  Returns (values year month day)."
  (let* ((z (+ z 719468))
         (era (floor z 146097))
         (doe (- z (* era 146097)))
         (yoe (floor (- (+ doe (floor doe 36524)) (+ (floor doe 1460) (floor doe 146096))) 365))
         (y (+ yoe (* era 400)))
         (doy (- doe (+ (* 365 yoe) (floor yoe 4) (- (floor yoe 100)))))
         (mp (floor (+ (* 5 doy) 2) 153)))
    (let ((d (+ (- doy (floor (+ (* 153 mp) 2) 5)) 1)))
      (if (< mp 10)
          (values y (+ mp 3) d)
          (values (1+ y) (- mp 9) d)))))

(defun %leap-year-p (y)
  (and (zerop (mod y 4)) (or (not (zerop (mod y 100))) (zerop (mod y 400)))))

(defun %days-in-month (y m)
  (case m
    (1 31) (2 (if (%leap-year-p y) 29 28)) (3 31) (4 30)
    (5 31) (6 30) (7 31) (8 31) (9 30) (10 31) (11 30) (12 31)))

(defun %iso-weekday (z)
  "ISO day-of-week of day number Z (0 = 1970-01-01, a Thursday): 1=Mon..7=Sun."
  (let ((w (mod (+ z 4) 7)))
    (if (zerop w) 7 w)))

(defun %week-of-year (z)
  "ISO 8601 week number of day number Z."
  (let* ((dow (%iso-weekday z))
         (thursday (+ z (- 4 dow)))     ; Thursday of this ISO week
         (y (nth-value 0 (%days-to-civil thursday)))
         (jan4 (%civil-to-days y 1 4))
         (w1-monday (- jan4 (- (%iso-weekday jan4) 1))))
    (multiple-value-bind (y m d) (%days-to-civil z)
      (declare (ignore m d))
      (values (1+ (floor (- z w1-monday) 7)) y))))

;;; ------------------------------------------------------------------
;;; ISO string parsing

(defun %iso-int (str start &optional (max 3))
  "Parse an integer at STR starting at START (possibly signed); returns
(values value end) or NIL when none."
  (let* ((i start) (n (length str)) (neg nil) (digits nil))
    (when (and (< i n) (member (char str i) '(#\+ #\-)))
      (setf neg (char= (char str i) #\-)) (incf i))
    (loop while (and (< i n) (digit-char-p (char str i)))
          do (push (char str i) digits) (incf i))
    (when digits
      (let ((v (parse-integer (coerce (nreverse digits) 'string))))
        (values (if neg (- v) v) i)))))

(defun %parse-iso-date (str)
  "Parse a date string (ISO 8601); returns (:cypher-date ...), possibly
with :week/:isoweekday for week dates.  Handles YYYY[-MM[-DD]], YYYYMMDD,
YYYYMM, YYYY-MM, week dates (YYYY-Www[-D]), ordinal dates (YYYY-DDD and
YYYYDDD), and signed extended years."
  (let* ((n (length str)) (i 0) (neg nil))
    (when (and (< i n) (member (char str i) '(#\+ #\-)))
      (setf neg (char= (char str i) #\-)) (incf i))
    (unless (and (<= (+ i 4) n) (every #'digit-char-p (coerce (subseq str i (+ i 4)) 'list)))
      (error "date: invalid year in ~s" str))
    (let* ((year (parse-integer (subseq str i (+ i 4))))
           (year (if neg (- year) year)))
      (incf i 4)
      (when (= i n) (return-from %parse-iso-date (list :cypher-date :year year :month 1 :day 1)))
      (let ((sep (and (< i n) (char= (char str i) #\-))))
        (when sep (incf i))
        (when (>= i n) (return-from %parse-iso-date (list :cypher-date :year year :month 1 :day 1)))
        (let ((c0 (char str i))
              (digits (loop with k = i while (and (< k n) (digit-char-p (char str k))) count t do (incf k))))
          (cond
            ((char= c0 #\W)
             (incf i)
             (let* ((wk (parse-integer (subseq str i (+ i 2))))
                    (day 1))
               (incf i 2)
               (when (and (< i n) (char= (char str i) #\-)) (incf i))
               (when (< i n)
                 (setf day (parse-integer (subseq str i (1+ i)))) (incf i))
               (when (< i n) (error "date: week date trailing ~s" str))
               (list :cypher-date :year year :month 0 :day 0 :week wk :isoweekday day)))
            ((and (not sep) (= digits 4))
             ;; YYYYMMDD
             (let ((month (parse-integer (subseq str i (+ i 2))))
                   (day (parse-integer (subseq str (+ i 2) (+ i 4)))))
               (incf i 4)
               (when (< i n) (error "date: trailing ~s" str))
               (list :cypher-date :year year :month month :day day)))
            ((and (not sep) (= digits 3))
             ;; YYYYDDD ordinal
             (let ((doy (parse-integer (subseq str i (+ i 3)))))
               (multiple-value-bind (y2 m d) (%days-to-civil (+ (%civil-to-days year 1 1) doy -1))
                 (list :cypher-date :year y2 :month m :day d))))
            ((and (not sep) (= digits 2))
             ;; YYYYMM
             (let ((month (parse-integer (subseq str i (+ i 2)))))
               (list :cypher-date :year year :month month :day 1)))
            ((and sep (= digits 3))
             ;; YYYY-DDD ordinal
             (let ((doy (parse-integer (subseq str i (+ i 3)))))
               (multiple-value-bind (y2 m d) (%days-to-civil (+ (%civil-to-days year 1 1) doy -1))
                 (list :cypher-date :year y2 :month m :day d))))
            ((and sep (<= digits 2) (plusp digits))
             ;; YYYY-MM[-DD]
             (let ((month (parse-integer (subseq str i (+ i digits)))))
               (incf i digits)
               (let ((day 1))
                 (when (and (< i n) (char= (char str i) #\-)) (incf i))
                 (when (and (< i n) (<= (+ i 2) n)
                            (every #'digit-char-p (coerce (subseq str i (+ i 2)) 'list)))
                   (setf day (parse-integer (subseq str i (+ i 2)))) (incf i 2))
                 (when (< i n) (error "date: trailing ~s" str))
                 (list :cypher-date :year year :month month :day day))))
            (t (error "date: cannot parse ~s" str))))))))f
(defun %resolve-date (d)
  "Resolve a parsed date plist (may carry :week/:isoweekday) into a clean
(:cypher-date :year :month :day)."
  (if (getf (cdr d) :week)
      (let* ((year (getf (cdr d) :year))
             (dow (or (getf (cdr d) :isoweekday) 1))
             (week (getf (cdr d) :week))
             (jan4 (%civil-to-days year 1 4))
             (w1 (- jan4 (- (%iso-weekday jan4) 1)))
             (z (+ w1 (* (1- week) 7) (1- dow))))
        (multiple-value-bind (y m d2) (%days-to-civil z)
          (list :cypher-date :year y :month m :day d2)))
      (list :cypher-date :year (getf (cdr d) :year)
            :month (getf (cdr d) :month) :day (getf (cdr d) :day))))

(defun %parse-time-part (str)
  "Parse an HH:MM[:SS[.ffffff][+ZZ[:ZZ[:SS]]|Z]] time string.
Returns (:hour :minute :second :nanosecond :offset) where OFFSET is
minutes east of UTC (a :timezone key holds the raw text)."
  (let* ((n (length str)) (i 0)
         (hour (parse-integer (subseq str 0 2)))
         (minute (parse-integer (subseq str 3 5)))
         (second 0) (nanosecond 0) (offset nil) (tz nil))
    (setf i 5)
    (when (and (< i n) (char= (char str i) #\:))
      (incf i)
      (setf second (parse-integer (subseq str i (+ i 2)))) (incf i 2)
      (when (and (< i n) (char= (char str i) #\.))
        (incf i)
        (let ((start i))
          (loop while (and (< i n) (digit-char-p (char str i))) do (incf i))
          (let* ((frac (subseq str start i))
                 (v (parse-integer frac)))
            (setf nanosecond (* v (expt 10 (- 9 (length frac)))))))))
    ;; timezone
    (when (< i n)
      (let ((c (char str i)))
        (cond ((char= c #\Z) (setf offset 0 tz "Z") (incf i))
              ((member c '(#\+ #\-))
               (let ((neg (char= c #\-)))
                 (incf i)
                 (let ((oh (parse-integer (subseq str i (+ i 2))))
                       (om 0) (os 0))
                   (incf i 2)
                   (when (and (< i n) (char= (char str i) #\:)) (incf i))
                   (when (and (< i n) (<= (+ i 2) n)
                              (every #'digit-char-p (coerce (subseq str i (+ i 2)) 'list)))
                     (setf om (parse-integer (subseq str i (+ i 2)))) (incf i 2))
                   (when (and (< i n) (char= (char str i) #\:))
                     (incf i)
                     (setf os (parse-integer (subseq str i (+ i 2)))) (incf i 2))
                   (setf offset (* (if neg -1 1) (+ (* oh 60) om (if (plusp os) 1 0)))))
                 (setf tz (subseq str (- (length str) 1)))))))
      (when (< i n) (error "time: trailing ~s" str)))
    (list :hour hour :minute minute :second second :nanosecond nanosecond
          :offset offset :timezone tz)))
(defun %date-from-string (str)
  (let ((d (%parse-iso-date str)))
    (%resolve-date d)))

(defun %localdatetime-from-string (str)
  "Parse YYYY-MM-DD[THH:MM[:SS[.fff]]][offset] as a localdatetime
(any offset is ignored, per openCypher localdatetime parsing)."
  (let* ((tpos (position #\T str))
         (dstr (subseq str 0 (or tpos (length str))))
         (d (%resolve-date (%parse-iso-date dstr))))
    (multiple-value-bind (h mi s ns off)
        (if tpos
            (let ((tp (%parse-time-part (subseq str (1+ tpos)))))
              (values (getf tp :hour) (getf tp :minute) (getf tp :second)
                      (getf tp :nanosecond) :ignored))
            (values 0 0 0 0 :ignored))
      (declare (ignore off))
      (list :cypher-localdatetime :year (getf (cdr d) :year) :month (getf (cdr d) :month)
            :day (getf (cdr d) :day) :hour h :minute mi :second s :nanosecond ns))))

(defun %datetime-from-string (str)
  (let* ((tpos (position #\T str))
         (dstr (subseq str 0 (or tpos (length str))))
         (d (%resolve-date (%parse-iso-date dstr))))
    (if (null tpos)
        (list :cypher-datetime :year (getf (cdr d) :year) :month (getf (cdr d) :month)
              :day (getf (cdr d) :day) :hour 0 :minute 0 :second 0 :nanosecond 0 :offset 0)
        (let ((tp (%parse-time-part (subseq str (1+ tpos)))))
          (list :cypher-datetime :year (getf (cdr d) :year) :month (getf (cdr d) :month)
                :day (getf (cdr d) :day) :hour (getf tp :hour) :minute (getf tp :minute)
                :second (getf tp :second) :nanosecond (getf tp :nanosecond)
                :offset (or (getf tp :offset) 0) :timezone (getf tp :timezone))))))

(defun %localtime-from-string (str)
  (let ((tp (%parse-time-part str)))
    (list :cypher-localtime :hour (getf tp :hour) :minute (getf tp :minute)
          :second (getf tp :second) :nanosecond (getf tp :nanosecond))))

(defun %time-from-string (str)
  (let ((tp (%parse-time-part str)))
    (list :cypher-time :hour (getf tp :hour) :minute (getf tp :minute)
          :second (getf tp :second) :nanosecond (getf tp :nanosecond)
          :offset (or (getf tp :offset) 0) :timezone (getf tp :timezone))))

(defun %parse-iso-duration (str)
  "Parse an ISO 8601 duration (P[n]Y[n]M[n]W[n]D[T[n]H[n]M[n]S])."
  (let ((months 0) (days 0) (nanos 0) (neg nil) (i 0) (n (length str)))
    (when (and (< i n) (char= (char str i) #\-)) (setf neg t) (incf i))
    (unless (and (< i n) (char= (char str i) #\P))
      (error "duration: must start with P"))
    (incf i)
    (loop
      (when (>= i n) (return))
      (if (char= (char str i) #\T)
          (progn (incf i)
                 (loop
                   (when (>= i n) (return))
                   (multiple-value-bind (val new-i) (%iso-int str i 6)
                     (unless new-i (error "duration: bad time field"))
                     (setf i new-i)
                     (when (>= i n) (error "duration: missing time unit"))
                     (let ((u (char str i))) (incf i)
                       (cond ((char= u #\H) (setf nanos (+ nanos (* val 3600 1000000000))))
                             ((char= u #\M) (setf nanos (+ nanos (* val 60 1000000000))))
                             ((char= u #\S) (setf nanos (+ nanos (round (* val 1000000000)))))
                             (t (error "duration: bad unit ~a" u)))))))
          (multiple-value-bind (val new-i) (%iso-int str i 8)
            (unless new-i (error "duration: bad field"))
            (setf i new-i)
            (when (>= i n) (error "duration: missing unit"))
            (let ((u (char str i))) (incf i)
              (cond ((char= u #\Y) (setf months (+ months (* val 12))))
                    ((char= u #\M) (setf months (+ months val)))
                    ((char= u #\W) (setf days (+ days (* val 7))))
                    ((char= u #\D) (setf days (+ days val)))
                    (t (error "duration: bad unit ~a" u)))))))
    (when (< i n) (error "duration: trailing ~s" str))
    (if neg
        (list :cypher-duration :months (- months) :days (- days) :nanos (- nanos))
        (list :cypher-duration :months months :days days :nanos nanos))))

;;; ------------------------------------------------------------------
;;; toString

(defun %pad (n width)
  (format nil "~v,'0d" width n))

(defun %date-to-string (v)
  (format nil "~a-~a-~a" (%pad (getf (cdr v) :year) 4) (%pad (getf (cdr v) :month) 2) (%pad (getf (cdr v) :day) 2)))

(defun %time-of-day-string (v)
  (let ((sec (getf (cdr v) :second)) (frac (or (getf (cdr v) :nanosecond) 0)))
    (if (and (zerop sec) (zerop frac))
        (format nil "~a:~a" (%pad (getf (cdr v) :hour) 2) (%pad (getf (cdr v) :minute) 2))
        (format nil "~a:~a:~a~a"
                (%pad (getf (cdr v) :hour) 2) (%pad (getf (cdr v) :minute) 2) (%pad sec 2)
                (if (zerop frac)
                    ""
                    (format nil ".~a" (let ((s (%pad frac 9)))
                                        (string-right-trim "0" s))))))))

(defun %offset-string (offset)
  (if (null offset) ""
      (let* ((abs (abs offset)) (h (floor abs 60)) (mm (mod abs 60)))
        (format nil "~a~a:~a" (if (minusp offset) "-" "+") (%pad h 2) (%pad mm 2)))))

(defun %temporal-to-string (v)
  (cond
    ((%date-p v) (%date-to-string v))
    ((%localtime-p v) (%time-of-day-string v))
    ((%time-p v) (concatenate 'string (%time-of-day-string v) (%offset-string (getf (cdr v) :offset))))
    ((%localdatetime-p v)
     (format nil "~aT~a" (%date-to-string v) (%time-of-day-string v)))
    ((%datetime-p v)
     (format nil "~aT~a~a" (%date-to-string v) (%time-of-day-string v)
             (%offset-string (getf (cdr v) :offset))))
    ((%duration-p v)
     (let ((m (getf (cdr v) :months)) (d (getf (cdr v) :days)) (ns (getf (cdr v) :nanos)))
       (let ((neg (or (minusp m) (minusp d) (minusp ns))))
         (let ((am (abs m)) (dd (abs d)) (ans (abs ns)))
           (let ((sec (floor ans 1000000000)) (an (mod ans 1000000000)))
             (let ((dur (with-output-to-string (o)
                          (write-string "P" o)
                          (when (plusp (floor am 12)) (format o "~aY" (floor am 12)))
                          (let ((ms (mod am 12)))
                            (when (plusp ms) (format o "~aM" ms)))
                          (let ((w (floor dd 7)) (rd (mod dd 7)))
                            (when (plusp w) (format o "~aW" w))
                            (when (plusp rd) (format o "~aD" rd)))
                          (when (or (plusp sec) (plusp an))
                            (write-char #\T o)
                            (when (plusp (floor sec 3600)) (format o "~aH" (floor sec 3600)))
                            (let ((mm2 (mod (floor sec 60) 60)))
                              (when (plusp mm2) (format o "~aM" mm2)))
                            (let ((ss (mod sec 60)))
                              (when (or (plusp ss) (plusp an))
                                (format o "~a~a" ss
                                        (if (zerop an) ""
                                            (format nil ".~a" (string-right-trim "0" (%pad an 9)))))
                                (write-char #\S o)))))))
               (if neg (concatenate 'string "-" dur) dur)))))))
    (t (princ-to-string v))))
;;; ------------------------------------------------------------------
;;; accessors (property components)

(defun %temporal-component (v name)
  "Accessor for a temporal value: .year .month .day .hour .minute .second
.millisecond .microsecond .nanosecond .offset .week .dayOfWeek .dayOfYear
.quarter .epochSeconds."
  (let* ((kind (car v))
         (has-date (member kind '(:cypher-date :cypher-localdatetime :cypher-datetime)))
         (has-time (member kind '(:cypher-localtime :cypher-time :cypher-localdatetime :cypher-datetime)))
         (y (getf (cdr v) :year)) (mo (getf (cdr v) :month)) (d (getf (cdr v) :day))
         (z (and has-date (%civil-to-days y mo d)))
         (comp (string-downcase name)))
    (flet ((need (what)
             (cypher-signal "InvalidArgumentType"
                            :detail (format nil "~a has no ~a" (cypher-type-name v) what))))
      (cond
        ((string= comp "year") (if has-date y (need "year")))
        ((string= comp "month") (if has-date mo (need "month")))
        ((string= comp "day") (if has-date d (need "day")))
        ((string= comp "hour") (if has-time (getf (cdr v) :hour) (need "hour")))
        ((string= comp "minute") (if has-time (getf (cdr v) :minute) (need "minute")))
        ((string= comp "second") (if has-time (getf (cdr v) :second) (need "second")))
        ((string= comp "millisecond") (if has-time (floor (getf (cdr v) :nanosecond) 1000000) (need "millisecond")))
        ((string= comp "microsecond") (if has-time (floor (getf (cdr v) :nanosecond) 1000) (need "microsecond")))
        ((string= comp "nanosecond") (if has-time (getf (cdr v) :nanosecond) (need "nanosecond")))
        ((string= comp "week") (if has-date (%week-of-year z) (need "week")))
        ((string= comp "dayofweek") (if has-date (%iso-weekday z) (need "dayOfWeek")))
        ((string= comp "dayofyear") (if has-date (1+ (- z (%civil-to-days y 1 1))) (need "dayOfYear")))
        ((string= comp "quarter") (if has-date (1+ (floor (1- mo) 3)) (need "quarter")))
        ((string= comp "offset")
         (if (member kind '(:cypher-time :cypher-datetime))
             (* (getf (cdr v) :offset) 60)
             (need "offset")))
        ((string= comp "timezone")
         (if (member kind '(:cypher-time :cypher-datetime))
             (or (getf (cdr v) :timezone) "")
             (need "timezone")))
        (t (need (format nil "component ~a" name)))))))

;;; ------------------------------------------------------------------
;;; comparison

;;; ------------------------------------------------------------------
;;; comparison

(defun %temporal-key (v)
  "A list making same-kind temporal values comparable."
  (cond ((%date-p v) (list 0 (getf (cdr v) :year) (getf (cdr v) :month) (getf (cdr v) :day)))
        ((%localtime-p v)
         (list 1 (getf (cdr v) :hour) (getf (cdr v) :minute) (getf (cdr v) :second) (getf (cdr v) :nanosecond)))
        ((%time-p v)
         (list 2 (getf (cdr v) :hour) (getf (cdr v) :minute) (getf (cdr v) :second) (getf (cdr v) :nanosecond)
               (getf (cdr v) :offset)))
        ((%localdatetime-p v)
         (list 3 (getf (cdr v) :year) (getf (cdr v) :month) (getf (cdr v) :day)
               (getf (cdr v) :hour) (getf (cdr v) :minute) (getf (cdr v) :second) (getf (cdr v) :nanosecond)))
        ((%datetime-p v)
         (list 4 (getf (cdr v) :year) (getf (cdr v) :month) (getf (cdr v) :day)
               (getf (cdr v) :hour) (getf (cdr v) :minute) (getf (cdr v) :second) (getf (cdr v) :nanosecond)
               (getf (cdr v) :offset)))
        ((%duration-p v)
         (list 5 (getf (cdr v) :months) (getf (cdr v) :days) (getf (cdr v) :nanos)))
        (t nil)))

(defun %temporal-same-kind-p (a b)
  (and (%temporal-p a) (%temporal-p b) (eq (car a) (car b))))

(defun %temporal-compare (a b)
  "Compare two same-kind temporal values: -1/0/1.  Expects same kind."
  (let* ((ka (%temporal-key a)) (kb (%temporal-key b)))
    (cond ((equal ka kb) 0)
          ((< (second ka) (second kb)) -1)
          ((> (second ka) (second kb)) 1)
          (t (loop for x in (cddr ka) for y in (cddr kb) with eq-so-far = t
                   do (if eq-so-far
                          (cond ((< x y) (return -1))
                                ((> x y) (return 1))
                                (t nil))
                          (return nil))
                   finally (return 0))))))

;;; ------------------------------------------------------------------
;;; duration arithmetic

(defun %duration-total-nanos (d)
  "Approximate total nanoseconds of a duration (months=30.44d)."
  (+ (* (getf (cdr d) :months) 30 24 3600 1000000000)
     (* (getf (cdr d) :days) 24 3600 1000000000)
     (getf (cdr d) :nanos)))

(defun %date-plus-duration (date dur)
  (let* ((years (floor (getf dur :months) 12))
         (months (mod (getf dur :months) 12))
         (y (+ (getf date :year) years))
         (m (+ (getf date :month) months)))
    (let* ((m1 (1- m)) (y1 (+ y (floor m1 12))) (m2 (1+ (mod m1 12)))
           (day (min (getf date :day) (%days-in-month y1 m2)))
           (date2 (list :cypher-date :year y1 :month m2 :day day)))
      ;; add days then seconds/nanos
      (let* ((z (%civil-to-days y1 m2 day))
             (z2 (+ z (getf dur :days)))
             (ns (getf dur :nanos)))
        (multiple-value-bind (y2 m3 d3) (%days-to-civil z2)
          (if (zerop ns)
              (list :cypher-date :year y2 :month m3 :day d3)
              ;; add sub-day seconds to a datetime
              (let* ((n (round (/ ns 1000000000))))
                (let ((total (+ (floor (/ (* (%civil-to-days y2 m3 d3) 86400) 1)) n)))
                  (multiple-value-bind (dd hh) (floor total 86400)
                    (multiple-value-bind (yy mm dd2) (%days-to-civil dd)
                      (let* ((hh2 (mod hh 86400)))
                        (list :cypher-datetime :year yy :month mm :day dd2
                              :hour (floor hh2 3600) :minute (mod (floor hh2 60) 60)
                              :second (mod hh2 60) :nanosecond 0 :offset 0))))))))))))

;;; ------------------------------------------------------------------
;;; truncate

(defun %truncate-temporal (v unit)
  "truncate(v, unit): truncate a temporal value to a precision unit."
  (let ((u (string-downcase unit)))
    (cond
      ((%date-p v)
       (case (intern u "KEYWORD")
         ((:year) (list :cypher-date :year (getf (cdr v) :year) :month 1 :day 1))
         ((:month) (list :cypher-date :year (getf (cdr v) :year) :month (getf (cdr v) :month) :day 1))
         ((:day :week) v)
         (t (cypher-signal "InvalidArgumentType" :detail (format nil "cannot truncate date to ~a" u)))))
      ((%localdatetime-p v)
       (flet ((base () (list :cypher-localdatetime :year (getf (cdr v) :year) :month (getf (cdr v) :month)
                             :day (getf (cdr v) :day) :hour 0 :minute 0 :second 0 :nanosecond 0)))
         (let ((y (getf (cdr v) :year)) (mo (getf (cdr v) :month)) (d (getf (cdr v) :day)))
           (case (intern u "KEYWORD")
             ((:year) (list :cypher-localdatetime :year y :month 1 :day 1 :hour 0 :minute 0 :second 0 :nanosecond 0))
             ((:month) (list :cypher-localdatetime :year y :month mo :day 1 :hour 0 :minute 0 :second 0 :nanosecond 0))
             ((:day) (base))
             ((:hour) (cons :cypher-localdatetime (append (list :year y :month mo :day d) (list :hour (getf (cdr v) :hour) :minute 0 :second 0 :nanosecond 0))))
             ((:minute) (cons :cypher-localdatetime (append (list :year y :month mo :day d) (list :hour (getf (cdr v) :hour) :minute (getf (cdr v) :minute) :second 0 :nanosecond 0))))
             ((:second) (cons :cypher-localdatetime (append (list :year y :month mo :day d) (list :hour (getf (cdr v) :hour) :minute (getf (cdr v) :minute) :second (getf (cdr v) :second) :nanosecond 0))))
             (t (cypher-signal "InvalidArgumentType" :detail (format nil "bad truncate unit ~a" u)))))))
      (t (cypher-signal "InvalidArgumentType" :detail (format nil "truncate on ~a" (cypher-type-name v)))))))
;;; ------------------------------------------------------------------
;;; constructors (called from %call-scalar)

(defun %now ()
  "Current local wall-clock broken down for date()/datetime() zero-arg."
  (multiple-value-bind (sec min hour day month year) (decode-universal-time (get-universal-time))
    (declare (ignore sec min hour))
    (values year month day)))

(defun %temporal-from-map (name mappairs)
  "Build a temporal value from a map of components."
  (flet ((get (k) (cdr (assoc k mappairs :test #'string=))))
    (let ((y (get "year")) (mo (get "month")) (d (get "day"))
          (h (get "hour")) (mi (get "minute")) (se (get "second"))
          (ns (get "nanosecond")) (off (get "offset")))
      (flet ((or2 (a b) (or a b)))
        (cond
          ((string-equal name "date")
           (list :cypher-date :year (or2 y 2000) :month (or2 mo 1) :day (or2 d 1)))
          ((string-equal name "localtime")
           (list :cypher-localtime :hour (or2 h 0) :minute (or2 mi 0) :second (or2 se 0) :nanosecond (or2 ns 0)))
          ((string-equal name "time")
           (list :cypher-time :hour (or2 h 0) :minute (or2 mi 0) :second (or2 se 0)
                 :nanosecond (or2 ns 0) :offset (or2 off 0)))
          ((string-equal name "localdatetime")
           (list :cypher-localdatetime :year (or2 y 2000) :month (or2 mo 1) :day (or2 d 1)
                 :hour (or2 h 0) :minute (or2 mi 0) :second (or2 se 0) :nanosecond (or2 ns 0)))
          ((string-equal name "datetime")
           (list :cypher-datetime :year (or2 y 2000) :month (or2 mo 1) :day (or2 d 1)
                 :hour (or2 h 0) :minute (or2 mi 0) :second (or2 se 0) :nanosecond (or2 ns 0)
                 :offset (or2 off 0)))
          (t (cypher-signal "InvalidArgumentType" :detail (format nil "~a(map)" name))))))))

(defun %temporal-call (name args row graph params)
  "Dispatch the openCypher temporal functions."
  (declare (ignore row graph params))
  (let ((n (length args)))
    (flet ((arg (i) (nth i args))
           (one () (first args)))
      (cond
        ;; truncate(v, unit)
        ((string-equal name "truncate")
         (if (= n 2)
             (%truncate-temporal (one) (second args))
             (%fn-error name args)))
        ;; current*: zero arg => now
        ((string-equal name "currentdate")
         (multiple-value-bind (y m d) (%now)
           (list :cypher-date :year y :month m :day d)))
        ((string-equal name "currenttime")
         (list :cypher-time :hour 0 :minute 0 :second 0 :nanosecond 0 :offset 0))
        ((string-equal name "currentdatetime")
         (multiple-value-bind (y m d) (%now)
           (list :cypher-datetime :year y :month m :day d :hour 0 :minute 0 :second 0 :nanosecond 0 :offset 0)))
        ((string-equal name "currenttimestamp")
         (get-universal-time))
        ;; zero-arg constructors
        ((zerop n)
         (multiple-value-bind (y m d) (%now)
           (cond ((string-equal name "date") (list :cypher-date :year y :month m :day d))
                 ((string-equal name "localtime") (list :cypher-localtime :hour 0 :minute 0 :second 0 :nanosecond 0))
                 ((string-equal name "time") (list :cypher-time :hour 0 :minute 0 :second 0 :nanosecond 0 :offset 0))
                 ((string-equal name "localdatetime") (list :cypher-localdatetime :year y :month m :day d :hour 0 :minute 0 :second 0 :nanosecond 0))
                 ((string-equal name "datetime") (list :cypher-datetime :year y :month m :day d :hour 0 :minute 0 :second 0 :nanosecond 0 :offset 0))
                 (t (cypher-signal "InvalidArgumentType" :detail (format nil "~a()" name))))))
        ((and (= n 3) (string-equal name "date"))
         (list :cypher-date :year (one) :month (arg 1) :day (arg 2)))
        ((= n 1)
         (let ((v (one)))
           (cond
             ((%tv-null v) :cypher-null)
             ((stringp v)
              (cond ((string-equal name "date") (%date-from-string v))
                    ((string-equal name "localtime") (%localtime-from-string v))
                    ((string-equal name "time") (%time-from-string v))
                    ((string-equal name "localdatetime") (%localdatetime-from-string v))
                    ((string-equal name "datetime") (%datetime-from-string v))
                    ((string-equal name "duration") (%parse-iso-duration v))
                    (t (%fn-error name args))))
             ((cypher-map-p v)
              (%temporal-from-map name (cypher-map-pairs v)))
             ((%temporal-p v)
              (t-of v name))  ; e.g. datetime(date(x))
             (t (%fn-error name args)))))
        (t (%fn-error name args))))))

(defun t-of (v name)
  "Convert between temporal types (e.g. datetime(date(x)) loses the time)."
  (cond
    ((string-equal name "date")
     (if (%date-p v) v
         (multiple-value-bind (y m d) (%days-to-civil
                                        (%civil-to-days (getf (cdr v) :year)
                                                       (getf (cdr v) :month) (getf (cdr v) :day)))
           (list :cypher-date :year y :month m :day d))))
    (t v)))

(defun %temporal-encode-ints (v)
  "Signed integers (first is the kind code) representing a temporal value."
  (let ((k (car v)) (plist (cdr v)))
    (case k
      (:cypher-date (list 0 (getf plist :year) (getf plist :month) (getf plist :day)))
      (:cypher-localtime (list 1 (getf plist :hour) (getf plist :minute) (getf plist :second) (getf plist :nanosecond)))
      (:cypher-time (list 2 (getf plist :hour) (getf plist :minute) (getf plist :second) (getf plist :nanosecond) (or (getf plist :offset) 0)))
      (:cypher-localdatetime (list 3 (getf plist :year) (getf plist :month) (getf plist :day)
                              (getf plist :hour) (getf plist :minute) (getf plist :second) (getf plist :nanosecond)))
      (:cypher-datetime (list 4 (getf plist :year) (getf plist :month) (getf plist :day)
                         (getf plist :hour) (getf plist :minute) (getf plist :second) (getf plist :nanosecond) (or (getf plist :offset) 0)))
      (:cypher-duration (list 5 (getf plist :months) (getf plist :days) (getf plist :nanos)))
      (t (error "codec: unknown temporal kind ~s" k)))))

(defun %un-uint (u) (if (logbitp 63 u) (- u #x10000000000000000) u))

(defun %temporal-from-encode-ints (ints)
  "Reconstruct a temporal value from its encoded integer list."
  (case (first ints)
    (0 (list :cypher-date :year (nth 1 ints) :month (nth 2 ints) :day (nth 3 ints)))
    (1 (list :cypher-localtime :hour (nth 1 ints) :minute (nth 2 ints) :second (nth 3 ints) :nanosecond (nth 4 ints)))
    (2 (list :cypher-time :hour (nth 1 ints) :minute (nth 2 ints) :second (nth 3 ints) :nanosecond (nth 4 ints) :offset (nth 5 ints)))
    (3 (list :cypher-localdatetime :year (nth 1 ints) :month (nth 2 ints) :day (nth 3 ints)
             :hour (nth 4 ints) :minute (nth 5 ints) :second (nth 6 ints) :nanosecond (nth 7 ints)))
    (4 (list :cypher-datetime :year (nth 1 ints) :month (nth 2 ints) :day (nth 3 ints)
             :hour (nth 4 ints) :minute (nth 5 ints) :second (nth 6 ints) :nanosecond (nth 7 ints) :offset (nth 8 ints)))
    (5 (list :cypher-duration :months (nth 1 ints) :days (nth 2 ints) :nanos (nth 3 ints)))
    (t (error "codec: unknown temporal encode kind ~a" (first ints)))))

