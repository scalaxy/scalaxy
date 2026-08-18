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
      (let ((tz-start i)
            (c (char str i)))
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
                 (setf tz (subseq str tz-start))))))
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

(defun %norm-dur (months days nanos)
  "Normalize possibly-fractional months/days (rationals) into integer months/days/nanos."
  (let* ((mi (floor months))
         (mf (- months mi))
         (days2 (+ days (* mf (/ 146097 4800))))
         (di (floor days2))
         (df (- days2 di)))
    (list :cypher-duration :months mi :days di
          :nanos (+ (or nanos 0) (round (* df 86400 1000000000))))))

(defun %parse-iso-duration (str)
  "Parse an ISO 8601 duration using exact rational arithmetic."
  (let ((months 0) (days 0) (nanos 0) (neg nil) (i 0) (n (length str)) (intime nil))
    (labels ((ch (k) (and (< k n) (char str k)))
             (isnum (c) (and c (digit-char-p c))))
      (when (and (< i n) (char= (ch i) (char "-" 0))) (setf neg t) (incf i))
      (unless (and (< i n) (char= (ch i) (char "P" 0))) (error "duration: must start with P"))
      (incf i)
      (flet ((rdn ()
               (let* ((s i) (ip (list nil)))
                 (loop while (and (< i n) (isnum (ch i))) do (incf i))
                 (let ((int (if (> i s) (parse-integer (subseq str s i)) 0))
                       (frac 0) (fd 0))
                   (when (and (< i n) (char= (ch i) (char "." 0)))
                     (incf i)
                     (let ((fstart i))
                       (loop while (and (< i n) (isnum (ch i))) do (incf i))
                       (setf fd (- i fstart))
                       (setf frac (parse-integer (subseq str fstart i)))))
                   (if (zerop fd) int (+ int (/ frac (expt 10 fd))))))))
        (loop
          (when (>= i n) (return))
          (when (char= (ch i) (char "T" 0)) (incf i) (setf intime t))
          (when (>= i n) (return))
          (let ((v (rdn)))
            (let ((u (and (< i n) (ch i))))
              (when u (incf i)
                (cond
                  ((char= u (char "Y" 0)) (setf months (+ months (* v 12))))
                  ((char= u (char "M" 0)) (if intime (setf nanos (+ nanos (round (* v 60000000000))))
                                             (setf months (+ months v))))
                  ((char= u (char "W" 0)) (setf days (+ days (* v 7))))
                  ((char= u (char "D" 0)) (setf days (+ days v)))
                  ((char= u (char "H" 0)) (setf nanos (+ nanos (round (* v 3600000000000)))))
                  ((char= u (char "S" 0)) (setf nanos (+ nanos (round (* v 1000000000)))))
                  (t nil))))))
        (let ((res (%norm-dur months days nanos)))
          (if neg
              (list :cypher-duration :months (- (getf res :months))
                    :days (- (getf res :days)) :nanos (- (getf res :nanos)))
              res))))))



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
      (if (zerop offset)
          "Z"
          (let* ((abs (abs offset)) (h (floor abs 60)) (mm (mod abs 60)))
            (format nil "~a~a:~a" (if (minusp offset) "-" "+") (%pad h 2) (%pad mm 2))))))

(defun %duration-to-string (v)
  (let ((months (getf (cdr v) :months))
        (days (getf (cdr v) :days))
        (nanos (getf (cdr v) :nanos)))
    (if (and (zerop months) (zerop days) (zerop nanos))
        "PT0S"
        (with-output-to-string (o)
      (write-string "P" o)
      ;; months -> years + months, shared sign
      (let ((mneg (minusp months)) (am (abs months)))
        (let ((years (floor am 12)) (mm (mod am 12))
              (sg (if mneg "-" "")))
          (when (plusp years) (format o "~a~aY" sg years))
          (when (plusp mm) (format o "~a~aM" sg mm))))
      ;; days, own sign
      (let ((dneg (minusp days)) (dd (abs days))
            (sg2 (if (minusp days) "-" "")))
        (when (plusp dd) (format o "~a~aD" sg2 dd)))
      ;; time, own sign, with 60-carry
      (unless (zerop nanos)
        (write-char #\T o)
        (let ((tsg (if (minusp nanos) "-" ""))
              (an (abs nanos)))
          (let ((sec (floor an 1000000000)) (frac (mod an 1000000000)))
            (let ((h (floor sec 3600))
                  (m2 (mod (floor sec 60) 60))
                  (s2 (mod sec 60)))
              (when (plusp h) (format o "~a~aH" tsg h))
              (when (plusp m2) (format o "~a~aM" tsg m2))
              (when (or (plusp s2) (plusp frac))
                (format o "~a~a~aS" tsg s2
                        (if (zerop frac) "" (format nil ".~a" (string-right-trim "0" (%pad frac 9))))))))))))))

(defun %temporal-to-string (v)
  (cond
    ((%date-p v) (%date-to-string v))
    ((%localtime-p v) (%time-of-day-string v))
    ((%time-p v)
     (concatenate 'string (%time-of-day-string v)
                  (let ((tz (getf (cdr v) :timezone)))
                    (if tz tz (%offset-string (getf (cdr v) :offset))))))
    ((%localdatetime-p v)
     (format nil "~aT~a" (%date-to-string v) (%time-of-day-string v)))
    ((%datetime-p v)
     (format nil "~aT~a~a" (%date-to-string v) (%time-of-day-string v)
             (let ((tz (getf (cdr v) :timezone)))
               (if tz tz (%offset-string (getf (cdr v) :offset))))))
    ((%duration-p v) (%duration-to-string v))
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
      (if (%duration-p v)
          ;; duration component accessors (Temporal5/10)
          (let ((months (getf (cdr v) :months))
                (days (getf (cdr v) :days))
                (nanos (getf (cdr v) :nanos)))
            (let ((sec (floor (/ nanos 1000000000)))
                  (sub (mod (/ nanos 1000000) 1000)))
              (cond
                ((string= comp "years") (floor months 12))
                ((string= comp "months") (mod months 12))
                ((string= comp "days") days)
                ((string= comp "hours") (floor (/ nanos 3600000000000)))
                ((string= comp "minutes") (mod (floor (/ nanos 60000000000)) 60))
                ((string= comp "seconds") (mod sec 60))
                ((string= comp "milliseconds") sub)
                ((string= comp "microseconds") (mod (floor (/ nanos 1000)) 1000))
                ((string= comp "nanoseconds") (mod nanos 1000))
                (t (need comp)))))
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
        (t (need (format nil "component ~a" name))))))))

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

(defun %truncate-components (y mo d h mi s ns unit)
  "Truncate calendar/time components to UNIT.  Returns (values y mo d h mi s ns)
with zeroed finer components."
  (case (intern (string-upcase unit) "KEYWORD")
    ((:millennium) (values (- y (mod y 1000)) 1 1 0 0 0 0))
    ((:century) (values (- y (mod y 100)) 1 1 0 0 0 0))
    ((:decade) (values (- y (mod y 10)) 1 1 0 0 0 0))
    ((:year) (values y 1 1 0 0 0 0))
    ((:quarter) (values y (+ (* (floor (1- mo) 3) 3) 1) 1 0 0 0 0))
    ((:month) (values y mo 1 0 0 0 0))
    ((:week)
     (let* ((z (%civil-to-days y mo d))
            (monday (- z (- (%iso-weekday z) 1))))
       (multiple-value-bind (w yy mm) (%days-to-civil monday)
         (declare (ignore w)) (values yy mm 0 0 0 0))))
    ((:day) (values y mo d 0 0 0 0))
    ((:hour) (values y mo d h 0 0 0))
    ((:minute) (values y mo d h mi 0 0))
    ((:second) (values y mo d h mi s 0))
    ((:millisecond) (values y mo d h mi s (* (floor ns 1000000) 1000000)))
    ((:microsecond) (values y mo d h mi s (* (floor ns 1000) 1000)))
    ((:nanosecond) (values y mo d h mi s ns))
    (t (cypher-signal "InvalidArgumentType" :detail (format nil "bad truncate unit ~a" unit)))))

(defun %build-truncated (result-kind y mo d h mi s ns offset)
  "Build a value of RESULT-KIND from components (rounding the day-of-week
result)."
  (case (intern (string-upcase result-kind) "KEYWORD")
    ((:date) (list :cypher-date :year y :month mo :day d))
    ((:localtime) (list :cypher-localtime :hour h :minute mi :second s :nanosecond ns))
    ((:time) (list :cypher-time :hour h :minute mi :second s :nanosecond ns :offset offset))
    ((:localdatetime) (list :cypher-localdatetime :year y :month mo :day d
                            :hour h :minute mi :second s :nanosecond ns))
    ((:datetime) (list :cypher-datetime :year y :month mo :day d
                       :hour h :minute mi :second s :nanosecond ns :offset offset))
    (t (cypher-signal "InvalidArgumentType" :detail (format nil "bad truncate type ~a" result-kind)))))

(defun %truncate-temporal (result-kind v unit map-pairs)
  "truncate(TYPE, unit, value, map): truncate VALUE to UNIT and build a
RESULT-KIND value, applying MAP (component overrides)."
  (let* ((y (getf (cdr v) :year)) (mo (or (getf (cdr v) :month) 1)) (d (or (getf (cdr v) :day) 1))
         (h (or (getf (cdr v) :hour) 0)) (mi (or (getf (cdr v) :minute) 0))
         (s2 (or (getf (cdr v) :second) 0)) (ns (or (getf (cdr v) :nanosecond) 0))
         (off (or (getf (cdr v) :offset) 0))
         (tz (getf (cdr v) :timezone)))
    (when (and map-pairs (%map-get map-pairs "timezone"))
      (setf off 0 tz (%map-get map-pairs "timezone")))
    (multiple-value-bind (ty tmo td th tmi ts tns) (%truncate-components y mo d h mi s2 ns unit)
      (when map-pairs
        (let ((my (%map-get map-pairs "year")) (mmo (%map-get map-pairs "month"))
              (md (%map-get map-pairs "day")) (mh (%map-get map-pairs "hour"))
              (mmi (%map-get map-pairs "minute")) (ms (%map-get map-pairs "second"))
              (mns (%map-get map-pairs "nanosecond")))
          (when my (setf ty my)) (when mmo (setf tmo mmo)) (when md (setf td md))
          (when mh (setf th mh)) (when mmi (setf tmi mmi)) (when ms (setf ts ms))
          (when mns (setf tns mns))))
      ;; the truncated day-of-week case: %truncate-components week returns day 0; recompute via civil
      (when (and (member (string-downcase unit) '("week") :test #'string-equal) (zerop td))
        ;; handled above; td should already be correct
        nil)
      (let ((res (%build-truncated result-kind ty tmo td th tmi ts tns off)))
        (if (and tz (or (%time-p res) (%datetime-p res)))
            (plist-put res :timezone tz)
            res)))))

(defun %now ()
  "Current local wall-clock broken down for date()/datetime() zero-arg."
  (multiple-value-bind (sec min hour day month year) (decode-universal-time (get-universal-time))
    (declare (ignore sec min hour))
    (values year month day)))

(defun %temporal-convert (name v)
  "Convert a temporal value V into type NAME (date/timetype/etc)."
  (cond
    ((string-equal name "date")
     (if (%date-p v) v
         (multiple-value-bind (y m d)
             (%days-to-civil (%civil-to-days (or (getf (cdr v) :year) 2000)
                                             (or (getf (cdr v) :month) 1)
                                             (or (getf (cdr v) :day) 1)))
           (list :cypher-date :year y :month m :day d))))
    ((string-equal name "localtime")
     (list :cypher-localtime :hour (getf (cdr v) :hour) :minute (getf (cdr v) :minute)
           :second (getf (cdr v) :second) :nanosecond (getf (cdr v) :nanosecond)))
    ((string-equal name "time")
     (list :cypher-time :hour (getf (cdr v) :hour) :minute (getf (cdr v) :minute)
           :second (getf (cdr v) :second) :nanosecond (getf (cdr v) :nanosecond)
           :offset (or (getf (cdr v) :offset) 0)))
    ((string-equal name "localdatetime")
     (if (or (%localdatetime-p v) (%datetime-p v)) v
         (list :cypher-localdatetime :year (getf (cdr v) :year) :month (getf (cdr v) :month)
               :day (getf (cdr v) :day) :hour (getf (cdr v) :hour) :minute (getf (cdr v) :minute)
               :second (getf (cdr v) :second) :nanosecond (getf (cdr v) :nanosecond))))
    ((string-equal name "datetime")
     (if (%datetime-p v) v
         (let ((off (or (getf (cdr v) :offset) 0)))
           (list :cypher-datetime :year (getf (cdr v) :year) :month (getf (cdr v) :month)
                 :day (getf (cdr v) :day) :hour (getf (cdr v) :hour) :minute (getf (cdr v) :minute)
                 :second (getf (cdr v) :second) :nanosecond (getf (cdr v) :nanosecond)
                 :offset off))))
    (t v)))

(defun %map-get (mappairs k) (cdr (assoc k mappairs :test #'string-equal)))


(defun %map-plural (mappairs)
  "Map plural construction keys to singular unless the singular exists."
  (let* ((plur (list (cons "years" "year") (cons "months" "month")
                     (cons "days" "day") (cons "hours" "hour")
                     (cons "minutes" "minute") (cons "seconds" "second")))
         (out (mapcar (lambda (q)
                        (let ((g (assoc (car q) plur :test #'string-equal)))
                          (if (and g (not (assoc (cdr g) mappairs :test #'string=)))
                              (cons (cdr g) (cdr q))
                              q)))
                      mappairs)))
    (nreverse out)))

(defun %map-fold-fields (mappairs)
  "Fold date/datetime/time field components into a construction MAP."
  (let ((out nil))
    (dolist (p mappairs)
      (let ((k (car p)) (v (cdr p)))
        (cond
          ((member k '("millisecond" "milliseconds") :test #'string-equal)
           (when (not (%map-get mappairs "nanosecond"))
             (push (cons "nanosecond" (round (* (or v 0) 1000000.0))) out)))
          ((member k '("microsecond" "microseconds") :test #'string-equal)
           (when (not (%map-get mappairs "nanosecond"))
             (push (cons "nanosecond" (round (* (or v 0) 1000.0))) out)))
          (t
        (if (and (or (string-equal k "datetime") (string-equal k "time"))
                 (%temporal-p v))
            (progn
              (when (and (getf (cdr v) :year) (not (%map-get mappairs "year")))
                (push (cons "year" (getf (cdr v) :year)) out))
              (when (and (getf (cdr v) :month) (not (%map-get mappairs "month")))
                (push (cons "month" (getf (cdr v) :month)) out))
              (when (and (getf (cdr v) :day) (not (%map-get mappairs "day")))
                (push (cons "day" (getf (cdr v) :day)) out))
              (when (and (getf (cdr v) :hour) (not (%map-get mappairs "hour")))
                (push (cons "hour" (getf (cdr v) :hour)) out))
              (when (and (getf (cdr v) :minute) (not (%map-get mappairs "minute")))
                (push (cons "minute" (getf (cdr v) :minute)) out))
              (when (and (getf (cdr v) :second) (not (%map-get mappairs "second")))
                (push (cons "second" (getf (cdr v) :second)) out))
              (when (and (getf (cdr v) :nanosecond) (not (%map-get mappairs "nanosecond")))
                (push (cons "nanosecond" (getf (cdr v) :nanosecond)) out))
              (when (and (getf (cdr v) :offset) (not (%map-get mappairs "offset")))
                (push (cons "offset" (getf (cdr v) :offset)) out)))
            (push p out))))))
    (nreverse out)))

(defun %map-collapse (mappairs)
  "Resolve week/ordinal-day/quarter based dates in a construction MAP,
replacing them with explicit year/month/day keys (leaving other keys)."
  (let ((y (%map-get mappairs "year"))
        (week (%map-get mappairs "week"))
        (dow (%map-get mappairs "dayOfWeek"))
        (od (%map-get mappairs "ordinalDay"))
        (dy (%map-get mappairs "dayOfYear"))
        (q (%map-get mappairs "quarter")))
    (if (and y (or week od dy q))
        (let ((yy y) (mm nil) (dd nil))
          (cond
            (week
             (let ((jan4 (%civil-to-days y 1 4)))
               (let ((w1 (- jan4 (- (%iso-weekday jan4) 1))))
                 (let ((z (+ w1 (* (1- week) 7) (1- (or dow 1)))))
                   (multiple-value-bind (wy wmo wd) (%days-to-civil z)
                     (setf yy wy mm wmo dd wd))))))
            ((or od dy)
             (let ((n (or od dy)))
               (multiple-value-bind (oy omo od2) (%days-to-civil (+ (%civil-to-days y 1 1) (1- n)))
                 (setf yy oy mm omo dd od2))))
            (q
             (let ((qday (or (%map-get mappairs "dayOfQuarter") 1)))
               (multiple-value-bind (qy qmo qd) (%days-to-civil (+ (%civil-to-days y (+ (* (1- q) 3) 1) 1) (1- qday)))
                 (setf yy qy mm qmo dd qd)))))
          (let ((out nil))
            (dolist (p mappairs)
              (unless (member (car p) '("week" "dayOfWeek" "ordinalDay" "dayOfYear" "quarter" "dayOfQuarter" "year") :test #'string-equal)
                (push p out)))
            (push (cons "year" yy) out)
            (push (cons "month" mm) out)
            (push (cons "day" dd) out)
            (nreverse out)))
        mappairs)))

(defun %temporal-from-map (name mappairs)
  "Build a temporal value from a map of components (may carry a
'date'/'time' field holding an existing temporal value)."
  (let ((dval (%map-get mappairs "date"))
        (tval (%map-get mappairs "time"))
        (y (%map-get mappairs "year")) (mo (%map-get mappairs "month")) (d (%map-get mappairs "day"))
        (h (%map-get mappairs "hour")) (mi (%map-get mappairs "minute")) (se (%map-get mappairs "second"))
        (ns (%map-get mappairs "nanosecond")) (off (%map-get mappairs "offset")))
    (when (and dval (%temporal-p dval))
      (unless y (setf y (getf (cdr dval) :year)))
      (unless mo (setf mo (getf (cdr dval) :month)))
      (unless d (setf d (getf (cdr dval) :day))))
    (when (and tval (%temporal-p tval))
      (unless h (setf h (getf (cdr tval) :hour)))
      (unless mi (setf mi (getf (cdr tval) :minute)))
      (unless se (setf se (getf (cdr tval) :second)))
      (unless ns (setf ns (getf (cdr tval) :nanosecond)))
      (unless off (setf off (getf (cdr tval) :offset))))
    (if (string-equal name "duration")
        ;; duration from map: years/months/days/hours/minutes/seconds/ms/us/ns
        (let ((years (or y 0))
              (mnths (or mo 0))
              (days (or d 0)))
          (let ((nanos (+ (* (or h 0) 3600 1000000000)
                          (* (or mi 0) 60 1000000000)
                          (* (or se 0) 1000000000)
                          (round (* (or (%map-get mappairs "milliseconds") 0) 1000000.0))
                          (round (* (or (%map-get mappairs "microseconds") 0) 1000.0))
                          (or ns 0))))
            (list :cypher-duration :months (+ (* years 12) mnths)
                  :days days :nanos nanos)))
        (let ((has-date (member name '("date" "datetime" "localdatetime") :test #'string-equal))
              (has-time (member name '("localtime" "time" "datetime" "localdatetime") :test #'string-equal)))
      (cond
        ((string-equal name "date")
         (list :cypher-date :year (or y 2000) :month (or mo 1) :day (or d 1)))
        ((string-equal name "localtime")
         (list :cypher-localtime :hour (or h 0) :minute (or mi 0) :second (or se 0) :nanosecond (or ns 0)))
        ((string-equal name "time")
         (list :cypher-time :hour (or h 0) :minute (or mi 0) :second (or se 0)
               :nanosecond (or ns 0) :offset (or off 0)))
        ((string-equal name "localdatetime")
         (list :cypher-localdatetime :year (or y 2000) :month (or mo 1) :day (or d 1)
               :hour (or h 0) :minute (or mi 0) :second (or se 0) :nanosecond (or ns 0)))
        ((string-equal name "datetime")
         (list :cypher-datetime :year (or y 2000) :month (or mo 1) :day (or d 1)
               :hour (or h 0) :minute (or mi 0) :second (or se 0) :nanosecond (or ns 0)
               :offset (or off 0)))
        (t (cypher-signal "InvalidArgumentType" :detail (format nil "~a(map)" name))))))))

(defun %temporal-call (name args row graph params)
  "Dispatch the openCypher temporal functions."
  (declare (ignore row graph params))
  (let ((n (length args)))
    (flet ((arg (i) (nth i args))
           (one () (first args)))
      (cond
        ;; static forms: date.truncate(...) / duration.between(...)
        ((search "." name)
         (%temporal-static-call name args))
        ;; truncate(v, unit)
        ((string-equal name "truncate")
         (if (= n 2)
             (%truncate-temporal (car (one)) (one) (second args) nil)
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
        ((and (= n 2) (member name '("datetime" "localdatetime") :test #'string-equal))
         ;; datetime(date, time) / localdatetime(date, time)
         (let ((d (one)) (t2 (arg 1)))
           (if (and (or (%date-p d) (%localdatetime-p d) (%datetime-p d))
                    (or (%time-p t2) (%localtime-p t2)))
               (let ((v (append (if (%date-p d) (cdr d) (if (%localdatetime-p d) (cdr d) (cdr d)))
                                (list :hour (getf (cdr t2) :hour)
                                      :minute (getf (cdr t2) :minute)
                                      :second (getf (cdr t2) :second)
                                      :nanosecond (getf (cdr t2) :nanosecond)))))
                 (if (string-equal name "datetime")
                     (cons :cypher-datetime (append v (list :offset (or (getf (cdr t2) :offset) 0))))
                     (cons :cypher-localdatetime v)))
               (cypher-signal "InvalidArgumentType" :detail "datetime needs date and time"))))
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
              (let ((mapped (%temporal-from-map name (%map-collapse (%map-fold-fields (%map-plural (cypher-map-pairs v)))))))
                (let ((tz (%map-get (cypher-map-pairs v) "timezone")))
                  (if (and tz (or (%time-p mapped) (%datetime-p mapped)))
                      (plist-put mapped :timezone tz)
                      mapped))))
             ((%temporal-p v)
              (t-of v name))  ; e.g. datetime(date(x))
             (t (%fn-error name args)))))
        (t (%fn-error name args))))))

(defun t-of (v name)
  "Convert between temporal types (e.g. datetime(date(x)) loses the time)."
  (%temporal-convert name v))

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



(defun %to-epoch-ns (v)
  "Absolute nanosecond timestamp from the epoch for a temporal value."
  (let ((k (car v)) (plist (cdr v)))
    (let ((days (if (member k '(:cypher-date :cypher-localdatetime :cypher-datetime))
                    (%civil-to-days (getf plist :year)
                                    (or (getf plist :month) 1)
                                    (or (getf plist :day) 1))
                    0))
          (secs (if (member k '(:cypher-localdatetime :cypher-datetime :cypher-time :cypher-localtime))
                    (+ (getf plist :hour) (* 60 (getf plist :minute)) (* 3600 (getf plist :second)))
                    0))
          (ns (if (member k '(:cypher-localdatetime :cypher-datetime :cypher-time :cypher-localtime))
                  (getf plist :nanosecond) 0)))
      (+ (* days 86400 1000000000) (* secs 1000000000) ns))))

(defun %sgn (neg x) (if neg (- (abs x)) (abs x)))

(defun %has-date-part (v) (or (%date-p v) (%localdatetime-p v) (%datetime-p v)))
(defun %has-time-part (v) (or (%localtime-p v) (%time-p v) (%localdatetime-p v) (%datetime-p v)))
(defun %date-y (v) (and (%has-date-part v) (getf (cdr v) :year)))
(defun %date-mo (v) (and (%has-date-part v) (or (getf (cdr v) :month) 1)))
(defun %date-d (v) (and (%has-date-part v) (or (getf (cdr v) :day) 1)))
(defun %lhs-before (a b) (< (%to-epoch-ns a) (%to-epoch-ns b)))
(defun %time-only (v)
  (if (%has-time-part v)
      (values (+ (* (getf (cdr v) :hour) 3600) (* (getf (cdr v) :minute) 60) (getf (cdr v) :second))
              (getf (cdr v) :nanosecond))
      (values 0 0)))

(defun %duration-between (lhs rhs &optional mode)
  "Duration from LHS to RHS.  MODE: nil (between) | :months | :days | :seconds."
  (unless (and (%temporal-p lhs) (%temporal-p rhs))
    (cypher-signal "InvalidArgumentType" :detail "duration.between needs two temporals"))
  (let ((neg (not (%lhs-before lhs rhs)))
        (ld (%has-date-part lhs)) (rd (%has-date-part rhs)))
    (let ((m 0) (day 0) (ns 0))
      (if (and ld rd)
          (let* ((y1 (%date-y lhs)) (mo1 (%date-mo lhs)) (d1 (%date-d lhs))
                 (y2 (%date-y rhs)) (mo2 (%date-mo rhs)) (d2 (%date-d rhs))
                 (tm (+ (* (- y2 y1) 12) (- mo2 mo1)))
                 (months (if (< d2 d1) (1- tm) tm))
                 (years (floor months 12))
                 (mm (mod months 12))
                 (bm (+ (1- mo1) mm))
                 (ay (+ y1 years (floor bm 12)))
                 (am (1+ (mod bm 12)))
                 (ad (min d1 (%days-in-month ay am)))
                 (ddd (- (%civil-to-days y2 mo2 d2) (%civil-to-days ay am ad))))
            (setf m months)
            (multiple-value-bind (s1 n1) (%time-only lhs)
              (multiple-value-bind (s2 n2) (%time-only rhs)
                (let ((tn (+ (* (- s2 s1) 1000000000) (- n2 n1))))
                  (setf day (+ ddd (floor tn (* 86400 1000000000)))
                        ns (mod tn (* 86400 1000000000)))))))
          ;; no date parts
          (multiple-value-bind (s1 n1) (%time-only lhs)
            (multiple-value-bind (s2 n2) (%time-only rhs)
              (let ((tn (+ (* (- s2 s1) 1000000000) (- n2 n1))))
                (setf ns tn)))))
      (case mode
        (:months (list :cypher-duration :months (%sgn neg m) :days 0 :nanos 0))
        (:days
         (if (and ld rd)
             ;; total day count between the two instants
             (list :cypher-duration :months 0
                   :days (%sgn neg (round (/ (- (%to-epoch-ns rhs) (%to-epoch-ns lhs))
                                             (* 86400 1000000000))))
                   :nanos 0)
             (list :cypher-duration :months 0 :days 0 :nanos 0)))
        (:seconds
         (if (and ld rd)
             (list :cypher-duration :months 0 :days 0
                   :nanos (if neg (- (abs (- (%to-epoch-ns rhs) (%to-epoch-ns lhs))))
                              (abs (- (%to-epoch-ns rhs) (%to-epoch-ns lhs)))))
             (list :cypher-duration :months 0 :days 0 :nanos (%sgn neg ns))))
        (t (list :cypher-duration :months (%sgn neg m) :days (%sgn neg day) :nanos (%sgn neg ns)))))))

(defun %temporal-static-call (name args)
  "Handle dotted static temporal functions: <type>.truncate(...) and
duration.between/inMonths/inDays/inSeconds(...)."
  (let ((dot (position #\. name)))
    (let ((type (subseq name 0 dot))
          (fn (subseq name (1+ dot))))
      (let ((a0 (first args)) (a1 (second args)) (a2 (third args)))
        (cond
          ((and (string-equal fn "truncate") args)
           ;; truncate(unit, value[, map]) - result type follows the prefix
           (let* ((unit (if (stringp a0) a0 (cypher-signal "InvalidArgumentType" :detail "truncate unit")))
                  (value (or a1 a0))
                  (map-pairs (and (third args) (cypher-map-p (third args))
                                  (cypher-map-pairs (third args)))))
             (%truncate-temporal type value unit map-pairs)))
          ((and (member fn '("between" "inmonths" "indays" "inseconds") :test #'string-equal)
                (>= (length args) 2))
           (let ((mode (cond ((string-equal fn "inmonths") :months)
                             ((string-equal fn "indays") :days)
                             ((string-equal fn "inseconds") :seconds)
                             (t nil))))
             (%duration-between a0 a1 mode)))
          (t (cypher-signal "InvalidArgumentType" :detail (format nil "unknown static ~a" name))))))))


(defun %temporal-static-name-p (name)
  "True when NAME is a dotted static temporal function
(<type>.truncate, duration.between/inMonths/inDays/inSeconds)."
  (and (position #\. name)
       (let ((dot (position #\. name)))
         (let ((type (subseq name 0 dot))
               (fn (subseq name (1+ dot))))
           (and (member type '("date" "time" "localtime" "datetime" "localdatetime" "duration")
                        :test #'string-equal)
                (or (and (string-equal fn "truncate"))
                    (and (string-equal type "duration")
                         (member (string-downcase fn)
                                 '("between" "inmonths" "indays" "inseconds")
                                 :test #'string-equal))))))))


(defun plist-put (plist key val)
  "Add KEY VAL to a marker-prefixed value plist (CAR is the marker)."
  (cons (car plist) (list* key val (cdr plist))))

