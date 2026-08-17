# Implementation Progress Log (objective: implement the Cypher plan)

## Done
- [x] Formal specs -> specs/ (openCypher9.pdf, TCK 1615 scenarios, BNF); specs/SOURCES.md
- [x] Plan document docs/cypher-implementation-plan.md
- [x] **Multi-database support**; **binary codec**; **graph storage** (blobs, indexes, invariants)
- [x] **Cypher front end** (conditions/lexer/parser/AST) — incl. chained comparisons, list/pattern comprehensions, map literals, DISTINCT aggregates, ON CREATE/ON MATCH, named paths, variable-length relationships
- [x] **Cypher executor** + reference oracle + differential tests — incl. the symbolic aggregation engine (aggregates inside expressions, implicit grouping, per-group ORDER BY), the path-chain BFS cursor, and the SET/MERGE effect algebra
- [x] **Semantic analysis** + **update clauses** — the openCypher error taxonomy incl. VariableAlreadyBound/VariableTypeConflict/AmbiguousAggregationExpression/ColumnNameConflict/NoExpressionAlias/NestedAggregation/NegativeIntegerArgument/NoVariablesInScope
- [x] **Wire protocol + integration** (+op-cypher+ 12, node dispatch, gateway-cypher, client cypher, /api/cypher, console; suite checks, 0 failures)
- [x] **TCK runner** (tests/tck.lisp + scripts/run-tck.lisp):
  - Gherkin-subset parser (Feature/Scenario/Outline/Examples/tags/steps/docstrings/tables, CRLF-tolerant)
  - scenario-outline expansion with placeholder substitution
  - step handlers for the full TCK vocabulary (graphs, parameters, queries, result tables in all order modes, side-effect accounting per the TCK observability definitions, error expectations)
  - TCK value parser (literals, entities, paths) + structural comparison (null == null, unordered lists, bag matching)
  - unsupported-feature classification with reasons
- [x] **docs/cypher-certification.md** — TCK report (3,897 scenarios: 2,247 pass, 505 fail, 1,145 unsupported-by-reason)
- [x] **docs/cypher-reference.md** — language reference for the supported subset

## Status
- Engine: unit suite green (9,018 checks, 0 failures)
- TCK: 3,897 scenarios executed; 2,247 pass; 505 fail (long-tail bugs in
  supported features); 1,145 unsupported (temporal 1,054, procedures 52,
  control-query 15, percentile 13, misc)
- Declared non-goals (see certification doc): temporal functions,
  stored procedures, percentile aggregates

- [x] **Benchmark dataset** — Neo4j Movie Graph (171 nodes / 253 rels,
      Apache-2.0) in benchmarks/movies/ + scripts/run-benchmark.lisp with a
      15-query suite (counts, filters, aggregation, paths, var-length)
- [x] **Large-scale benchmark dataset** — NYC taxi graph (263 zones, up to
      2.93M TRIP relationships from real TLC data) in benchmarks/nyc-taxi/ +
      scripts/run-benchmark-nyc.lisp, with a reproducible prepare.py
      pipeline and aggregated/per-trip modes

## Remaining (future work)
- Streaming aggregation: MATCH currently materializes rows before
  aggregation, so whole-graph aggregations over the full 2.93M-edge taxi
  graph need a large heap (see benchmarks/nyc-taxi/README.md)
- Temporal value types and functions (date/time/datetime/localtime/
  localdatetime/duration) — the largest remaining TCK block
- The long tail of failing scenarios in specs/tck-results.txt
- README/CHANGELOG references to the graph layer
