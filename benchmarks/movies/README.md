# Movie Graph Benchmark

A small, real-world graph dataset with a Cypher query suite, used to
benchmark the Scalaxy Cypher engine.

## Dataset

The **Neo4j Movie Graph** — the canonical tutorial graph of movies, actors,
directors, and their relationships:

| Metric | Value |
|---|---|
| Nodes | 171 |
| Relationships | 253 |
| Node labels | `Movie` (38), `Person` (133) |
| Relationship types | `ACTED_IN`, `DIRECTED`, `PRODUCED`, `WROTE`, `FOLLOWS`, `REVIEWED` |

### Provenance

- Source: <https://github.com/neo4j-graph-examples/movies> (`scripts/movies.cypher`)
- Downloaded: 2026-08-16 (raw file at `neo4j-graph-examples/movies@main`)
- License: Apache License 2.0 (the neo4j-graph-examples repository)

The dataset is redistributed here unmodified as `movies.cypher` for
benchmarking purposes.  Its two `CREATE CONSTRAINT` statements (schema
uniqueness constraints) are not part of the Scalaxy engine and are skipped by
the loader; the `MERGE`-based data loading is otherwise run as-is.

## Files

- `movies.cypher` — the dataset (Neo4j import script).
- `queries.lisp` — `*movie-benchmark-queries*`, a list of
  `(description . cypher)` covering counts, filters, projections,
  `ORDER BY`/`LIMIT`, aggregation (`count`/`avg`/`collect`), paths,
  variable-length relationships, and string predicates.

## Run

```sh
# load the dataset and time every query (20 iterations by default)
sbcl --script scripts/run-benchmark.lisp

# choose the number of iterations per query
sbcl --script scripts/run-benchmark.lisp 100
```

The runner prints the load time, graph size, and a table of
`query | rows | median | iterations` for each query in the suite.

## Results (reference)

On an Apple Silicon MacBook (SBCL 2.6.1, single thread):

```
load: 39 statements (2 schema statements skipped) in ~41 ms
nodes: 171, relationships: 253

count movies                1 row    0.06 ms
count people                1 row    0.14 ms
labels overview             2 rows   0.23 ms
actors of The Matrix        5 rows   1.02 ms
most prolific actors        5 rows   1.05 ms
most prolific writers       5 rows   0.51 ms
actors who directed themselves  3 rows   3.05 ms
top-rated movies by reviews 3 rows   0.52 ms
follows graph               3 rows   0.48 ms
largest casts (collect)     3 rows   0.88 ms
recent movies with directors 20 rows 0.62 ms
titles starting with 'The ' 9 rows   0.07 ms
filmography of Tom Hanks   12 rows   0.21 ms
co-actors within 3 hops    10 rows   0.51 ms
movies by decade            5 rows   0.08 ms
```
