;;;; benchmarks/nyc-taxi/queries.lisp --- NYC taxi graph benchmark queries
;;;; Each entry is (description . cypher).  Two suites: one for the
;;;; zone-pair-aggregated graph, one for the per-trip graph.

(in-package #:scalaxy)

(defparameter *nyc-taxi-queries-aggregated*
  (list
   (cons "count zones"
         "MATCH (z:Zone) RETURN count(*) AS zones")
   (cons "count trip relationships"
         "MATCH (:Zone)-[r:TRIP]->(:Zone) RETURN count(*) AS routes")
   (cons "total trips"
         "MATCH (:Zone)-[r:TRIP]->(:Zone) RETURN sum(r.trips) AS trips")
   (cons "busiest pickup zones"
         "MATCH (a:Zone)-[r:TRIP]->(:Zone) RETURN a.zone AS zone, sum(r.trips) AS trips ORDER BY trips DESC, zone LIMIT 5")
   (cons "busiest route"
         "MATCH (a:Zone)-[r:TRIP]->(b:Zone) RETURN a.zone AS from, b.zone AS to, r.trips AS trips ORDER BY trips DESC LIMIT 5")
   (cons "outgoing flow of JFK Airport"
         "MATCH (a:Zone {zone: 'JFK Airport'})-[r:TRIP]->(b:Zone) RETURN b.zone AS to, sum(r.trips) AS trips ORDER BY trips DESC LIMIT 5")
   (cons "zones by borough"
         "MATCH (z:Zone) RETURN z.borough AS borough, count(*) AS zones ORDER BY zones DESC")
   (cons "passenger volume by pickup zone"
         "MATCH (a:Zone)-[r:TRIP]->(:Zone) RETURN a.zone AS zone, sum(r.passengers) AS pax ORDER BY pax DESC, zone LIMIT 5")
   (cons "total fare revenue"
         "MATCH (:Zone)-[r:TRIP]->(:Zone) RETURN sum(r.fare) AS revenue")
   (cons "JFK to Times Sq within 2 hops"
         "MATCH p = (a:Zone {zone: 'JFK Airport'})-[:TRIP*1..2]->(b:Zone {zone: 'Times Sq/Theatre District'}) RETURN length(p) AS hops, count(*) AS routes ORDER BY hops")))

(defparameter *nyc-taxi-queries-per-trip*
  (list
   (cons "count zones"
         "MATCH (z:Zone) RETURN count(*) AS zones")
   (cons "count trip relationships"
         "MATCH (:Zone)-[r:TRIP]->(:Zone) RETURN count(*) AS trips")
   (cons "busiest pickup zones"
         "MATCH (a:Zone)-[:TRIP]->(:Zone) RETURN a.zone AS zone, count(*) AS trips ORDER BY trips DESC, zone LIMIT 5")
   (cons "busiest route"
         "MATCH (a:Zone)-[:TRIP]->(b:Zone) RETURN a.zone AS from, b.zone AS to, count(*) AS trips ORDER BY trips DESC LIMIT 5")
   (cons "average fare by pickup zone"
         "MATCH (a:Zone)-[r:TRIP]->(:Zone) RETURN a.zone AS zone, avg(r.fare) AS avg_fare, count(*) AS n ORDER BY n DESC, zone LIMIT 5")
   (cons "passenger volume by pickup zone"
         "MATCH (a:Zone)-[r:TRIP]->(:Zone) RETURN a.zone AS zone, sum(r.passengers) AS pax ORDER BY pax DESC, zone LIMIT 5")
   (cons "zones by borough"
         "MATCH (z:Zone) RETURN z.borough AS borough, count(*) AS zones ORDER BY zones DESC")
   (cons "total distance"
         "MATCH (:Zone)-[r:TRIP]->(:Zone) RETURN sum(r.distance) AS distance")
   (cons "zones reachable from JFK"
         "MATCH (a:Zone {zone: 'JFK Airport'})-[:TRIP]->(b:Zone) RETURN DISTINCT b.zone AS zone ORDER BY zone LIMIT 10")))
