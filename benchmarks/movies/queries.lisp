;;;; benchmark/movies queries -- the Neo4j Movie Graph query suite
;;;; Each entry is (description . cypher).

(in-package #:scalaxy)

(defparameter *movie-benchmark-queries*
  (list
   (cons "count movies"
         "MATCH (m:Movie) RETURN count(*) AS movies")
   (cons "count people"
         "MATCH (p:Person) RETURN count(*) AS people")
   (cons "labels overview"
         "MATCH (n) RETURN labels(n) AS label, count(*) AS n ORDER BY n DESC")
   (cons "actors of The Matrix"
         "MATCH (p:Person)-[:ACTED_IN]->(m:Movie) WHERE m.title = 'The Matrix' RETURN p.name AS name ORDER BY name")
   (cons "most prolific actors"
         "MATCH (p:Person)-[:ACTED_IN]->(m:Movie) RETURN p.name AS name, count(*) AS n ORDER BY n DESC, name LIMIT 5")
   (cons "most prolific writers"
         "MATCH (p:Person)-[:WROTE]->(m:Movie) RETURN p.name AS writer, count(*) AS n ORDER BY n DESC, writer LIMIT 5")
   (cons "actors who directed themselves"
         "MATCH (p:Person)-[:ACTED_IN]->(m:Movie)<-[:DIRECTED]-(d:Person) WHERE p.name = d.name RETURN DISTINCT p.name AS name")
   (cons "top-rated movies by reviews"
         "MATCH (p:Person)-[r:REVIEWED]->(m:Movie) RETURN m.title AS title, avg(r.rating) AS avg, count(*) AS n ORDER BY avg DESC LIMIT 3")
   (cons "follows graph"
         "MATCH (p:Person)-[:FOLLOWS]->(p2:Person) RETURN p.name AS a, p2.name AS b ORDER BY a, b")
   (cons "largest casts (collect)"
         "MATCH (m:Movie)<-[:ACTED_IN]-(p:Person) WITH m, collect(p.name) AS cast RETURN m.title AS title, size(cast) AS n ORDER BY n DESC, title LIMIT 3")
   (cons "recent movies with directors"
         "MATCH (p:Person)-[:DIRECTED]->(m:Movie) WHERE m.released >= 2000 RETURN p.name AS director, m.title AS title, m.released AS year ORDER BY year, title")
   (cons "titles starting with 'The '"
         "MATCH (m:Movie) WHERE m.title STARTS WITH 'The ' RETURN m.title AS title ORDER BY title")
   (cons "filmography of Tom Hanks"
         "MATCH (p:Person {name: 'Tom Hanks'})-[:ACTED_IN]->(m:Movie) RETURN m.title AS title, m.released AS year ORDER BY year")
   (cons "co-actors within 3 hops"
         "MATCH p = (a:Person {name: 'Keanu Reeves'})-[:ACTED_IN*1..3]-(b:Person) WHERE b.name <> 'Keanu Reeves' RETURN b.name AS name, length(p) AS hops ORDER BY hops, name LIMIT 10")
   (cons "movies by decade"
         "MATCH (m:Movie) RETURN toInteger(m.released / 10) * 10 AS decade, count(*) AS n ORDER BY decade")))
