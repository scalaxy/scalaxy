;;;; benchmarks/nyc-taxi/load.lisp --- load the NYC taxi graph into Scalaxy
;;;;
;;;; Nodes: (:Zone) with {id, borough, zone} (TLC taxi zones, 1..263).
;;;; Relationships: [:TRIP] pickup-zone -> dropoff-zone.
;;;;   per-trip mode:      {distance, fare, passengers} per trip
;;;;   aggregated mode:    {trips, distance, fare, passengers} per zone pair

(in-package #:scalaxy)

(defun %parse-decimal (s)
  "Fast decimal parser: '123.45' -> 123.45."
  (let ((dot (position #\. s))
        (n (length s)))
    (if dot
        (let* ((ip (parse-integer s :end dot :junk-allowed t))
               (fp (parse-integer s :start (1+ dot) :junk-allowed t))
               (digits (- n dot 1))
               (frac (if fp fp 0)))
          (+ (float (or ip 0)) (/ (float frac) (expt 10.0 digits))))
        (float (or (parse-integer s :junk-allowed t) 0)))))

(defun %load-nyc-zones (graph path)
  "Load the TLC taxi zones (zones.csv: id,borough,zone,service_zone).
Returns a hash-table mapping LocationID -> engine element id."
  (let ((by-id (make-hash-table :test #'equal)))
    (with-open-file (in path :external-format :utf-8)
      (read-line in)  ; header
      (loop for line = (read-line in nil :eof)
            until (eq line :eof)
            do (let ((f (split-sequence-on #\, line)))
                 (when (>= (length f) 3)
                   (let* ((zid (parse-integer (first f) :junk-allowed t))
                          (borough (second f))
                          (zone (third f)))
                     (when zid
                       (setf (gethash zid by-id)
                             (graph-create-node
                              graph
                              :labels '("Zone")
                              :props (list (cons "id" zid)
                                           (cons "borough" borough)
                                           (cons "zone" zone))))))))))
    by-id))

(defun %load-nyc-trips (graph by-id path &key (limit nil))
  "Load per-trip TRIP relationships from trips.csv
(pickup,dropoff,distance,fare,passengers).  Returns the number loaded."
  (let ((n 0))
    (with-open-file (in path :external-format :utf-8)
      (read-line in)  ; header
      (loop for line = (read-line in nil :eof)
            until (or (eq line :eof) (and limit (>= n limit)))
            do (let ((f (split-sequence-on #\, line)))
                 (when (>= (length f) 3)
                   (let ((pu (parse-integer (first f) :junk-allowed t))
                         (do (parse-integer (second f) :junk-allowed t)))
                     (when (and pu do
                                (gethash pu by-id) (gethash do by-id))
                       (graph-create-relationship
                        graph "TRIP" (gethash pu by-id) (gethash do by-id)
                        :props (list (cons "distance" (%parse-decimal (third f)))
                                     (cons "fare" (%parse-decimal (fourth f)))
                                     (cons "passengers"
                                           (parse-integer (fifth f) :junk-allowed t))))
                       (incf n)))))))
    n))

(defun %load-nyc-trips-aggregated (graph by-id path &key (limit nil))
  "Load aggregated TRIP relationships from trips_aggregated.csv
(pickup,dropoff,trips,distance,fare,passengers)."
  (let ((n 0))
    (with-open-file (in path :external-format :utf-8)
      (read-line in)
      (loop for line = (read-line in nil :eof)
            until (or (eq line :eof) (and limit (>= n limit)))
            do (let ((f (split-sequence-on #\, line)))
                 (when (>= (length f) 4)
                   (let ((pu (parse-integer (first f) :junk-allowed t))
                         (do (parse-integer (second f) :junk-allowed t)))
                     (when (and pu do (gethash pu by-id) (gethash do by-id))
                       (graph-create-relationship
                        graph "TRIP" (gethash pu by-id) (gethash do by-id)
                        :props (list (cons "trips"
                                           (parse-integer (third f) :junk-allowed t))
                                     (cons "distance" (%parse-decimal (fourth f)))
                                     (cons "fare" (%parse-decimal (fifth f)))
                                     (cons "passengers"
                                           (parse-integer (sixth f) :junk-allowed t))))
                       (incf n)))))))
    n))

(defun load-nyc-taxi (graph directory &key (mode :aggregated) (limit nil))
  "Load the NYC taxi graph from DIRECTORY.  MODE is :aggregated or
:per-trip; LIMIT caps the number of trip relationships.  Returns
(values zones trips)."
  (let* ((by-id (%load-nyc-zones graph (merge-pathnames "zones.csv" directory)))
         (trips (if (eq mode :per-trip)
                    (%load-nyc-trips graph by-id
                                     (merge-pathnames "trips.csv" directory)
                                     :limit limit)
                    (%load-nyc-trips-aggregated
                     graph by-id
                     (merge-pathnames "trips_aggregated.csv" directory)
                     :limit limit))))
    (values (hash-table-count by-id) trips)))
