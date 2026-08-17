# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Fixed

- **openCypher TCK conformance**: 2,252 → **2,726 passing** scenarios
  (failures 500 → 6) across the whole corpus — 3,898 scenarios executed.
  Notable clusters: pattern relationship uniqueness (self-loops, anonymous
  rels, MERGE duplicate creation); var-length patterns over a bound
  relationship list; ORDER BY aggregation scope rules (projected
  aggregates, grouping-key expressions, DISTINCT); EXISTS { } subqueries
  (WHERE clauses, aggregation scoping, bare patterns); SET/REMOVE/CREATE
  semantics (`SET n.p = null` removes, multi-label SET/REMOVE,
  parenthesized targets, `InvalidPropertyType`); DELETE of paths and
  label/type expressions; literal lexing (int64 ranges, float overflow,
  unicode punctuation); function semantics (`toString(null)`,
  `type()` on deleted rels, path equality, `min`/`max` total order,
  `UnknownFunction`); and TCK-harness fidelity (gherkin raw docs/tables,
  nested entity lists/maps, NaN cells, bag-match argument order).  See
  `docs/cypher-certification.md`.

## [1.8.0] - 2026-08-17

### Added

- Graph database layered on the replicated key/value store, with the
  **openCypher query language** implemented fully in Common Lisp (SBCL),
  zero external dependencies:
  - property-graph storage over the KV store: nodes, relationships,
    labels/types, properties, adjacency/type/label indexes, id minting, and
    blob spills for large binary properties (`src/graph.lisp`,
    `src/db.lisp`, `src/codec.lisp`);
  - the Cypher compiler pipeline (`src/cypher/`): lexer, parser, AST with
    canonical printer, semantic analysis, expression evaluator, updates,
    executor, wire format, and a metacircular reference oracle for
    differential testing;
  - the openCypher core: `MATCH` / `OPTIONAL MATCH` / `WHERE` / `WITH` /
    `RETURN` / `UNWIND` / `ORDER BY` / `SKIP` / `LIMIT` / `DISTINCT` /
    `UNION`, `CREATE` / `MERGE … ON CREATE/ON MATCH` / `SET` (incl. map
    `=`/`+=` with null-removal) / `REMOVE` / `DELETE` / `DETACH DELETE`;
  - named paths (`MATCH p = …`) and variable-length relationships
    (`-[:T*1..3]->`), with relationship-list binding;
  - full core expressions: list comprehensions, pattern comprehensions,
    list predicates (`all`/`any`/`none`/`single`), `EXISTS { }`, `CASE`,
    chained comparisons, maps, list concatenation/append/prepend;
  - symbolic aggregation: aggregates nested in arbitrary expressions
    (`count(a) * 10 + count(b) * 5`, `head(collect(…))`), implicit
    grouping, `DISTINCT` aggregates, per-group `ORDER BY`;
  - the openCypher error taxonomy: ~35 named error kinds as CLOS
    conditions;
  - cluster/API/web integration: `+op-cypher+` wire opcode, gateway
    routing, client `cypher`, `POST /api/cypher`, web-console command.
- **openCypher TCK runner** (`tests/tck.lisp`, `scripts/run-tck.lisp`):
  Gherkin-subset parser, scenario-outline expansion, side-effect
  accounting per the TCK observability definitions, result-table
  comparison (any-order/ordered/list-order-insensitive), error-expectation
  matching, and unsupported-feature classification.  3,897 scenarios
  executed; see `docs/cypher-certification.md`.
- Documentation: `docs/cypher-implementation-plan.md` (axiomatic design),
  `docs/cypher-reference.md` (language reference),
  `docs/cypher-certification.md` (conformance report).
- Benchmark datasets (see `benchmarks/` and
  `scripts/run-benchmark*.lisp`):
  - the Neo4j **Movie Graph** (171 nodes / 253 relationships; Apache-2.0)
    with a 15-query suite — counts, filters, aggregation, paths,
    var-length relationships;
  - the **NYC taxi graph** built from NYC TLC trip records (263 taxi-zone
    nodes; zone-pair aggregated and per-trip modes, up to 2,933,097
    `TRIP` relationships for one month), with a reproducible `prepare.py`
    pipeline and a two-mode query suite.

### Changed

- Queries without a `RETURN` now produce no result rows (openCypher
  semantics for updating queries).
- `SET` refreshes bound entity values in the rows (so `SET … WITH …
  WHERE …` sees the new values).
- Unit suite now runs 9,018 checks.

## [1.6.7] - 2026-08-07

First open-source release.

### Added

- Distributed key/value core in Common Lisp (SBCL):
  - consistent-hash sharding with virtual nodes (FNV-1a 64-bit + SplitMix64
    finalizer), so adding/removing a node moves only ~1/N of the keys,
  - durable append-only storage log (same record format as the network
    protocol) with exact replay on startup,
  - synchronous leader replication with per-write acks and read failover
    to replica holders,
  - key re-homing on node removal (no data loss).
- Web console written in Common Lisp with zero external dependencies:
  - dashboard (overview, data browser, cluster view, command console),
  - JSON REST API and `/healthz` endpoint,
  - dependency-free JSON encoder/decoder and HTTP/1.1 server/client.
- Cluster gateway: ring routing over TCP, per-peer status aggregation, and
  node-failure failover.
- Deployment:
  - multi-stage Dockerfile (runs the full test suite at build time),
  - docker-compose three-node cluster,
  - Kubernetes manifests (StatefulSet, PVCs, probes, headless service),
  - environment-variable configuration (`SCALAXY_*`).
- Hostname resolution for container/Kubernetes peer discovery; stable node
  identity and log filename across restarts.
- Test suite: 8,654 checks across 13 groups (storage, protocol,
  replication, clustering, TCP, HTTP, JSON, gateway routing, failover,
  hostname resolution).

### Fixed

- Consistent-hash ring distribution bias (SplitMix64 finalizer).
- Replication of writes received over the TCP data plane.
- Data loss on node removal (key re-homing) and on restart (stable
  `scalaxy.log` filename).
- Web console: Enter now runs console commands; stale UI assets are
  cache-busted.

### Security

- See [SECURITY.md](SECURITY.md) for the reporting process and design notes.
