# Cypher Reference — the Scalaxy Graph Query Language

Scalaxy ships a graph database layered on its replicated key/value store,
queried with the **openCypher** language.  This document is the reference for
the supported subset; the formal design (axioms, algebra, and compiler
pipeline) is in [cypher-implementation-plan.md](cypher-implementation-plan.md),
and the conformance report is in
[cypher-certification.md](cypher-certification.md).

## Concepts

- **Property graph.** Nodes and relationships with labels/types and
  string-keyed properties.  Values are the Cypher value lattice: `NULL`,
  booleans, integers, floats, strings, lists, maps, nodes, relationships,
  and paths (CL `NIL` is *never* a Cypher value — `NULL` is the keyword
  `:CYPHER-NULL`, `FALSE` is `:CYPHER-FALSE`).
- **Graphs are views over the KV store.** Every node, relationship,
  property, label, and adjacency index is a derived pure view of the
  replicated log, so durability and replication are inherited.
- **Queries are data.** The pipeline `string -> tokens -> AST -> semantic
  AST -> execution` is a chain of pure functions; any stage can be printed
  with `AST-PRINT` (see the plan document, §6).

## Quick start

```lisp
(require :asdf)
(asdf:load-asd "/path/to/scalaxy/scalaxy.asd")
(asdf:load-system "scalaxy")

(let* ((store (scalaxy:make-store))
       (graph (scalaxy:make-local-graph store)))
  (scalaxy:cypher-query "CREATE (ada:Person {name: 'Ada'})-[:KNOWS]->(bob:Person {name: 'Bob'})" graph)
  (scalaxy:cypher-query "MATCH (a:Person)-[:KNOWS]->(b) RETURN a.name AS a, b.name AS b" graph))
;; => (((|a| . "Ada") (|b| . "Bob")))
```

`SCALAXY:CYPHER-QUERY query graph &key params matcher` evaluates a query
string (or parsed AST) against a graph view and returns the result rows as a
list of alists (`(column-symbol . value)`).  `:matcher :reference` selects the
metacircular oracle used for differential testing.

## Clauses

| Clause | Notes |
|---|---|
| `MATCH` | node/relationship patterns, multi-chain, labels, types, directions, property predicates, named paths, variable-length relationships |
| `OPTIONAL MATCH` | unmatched variables bind to `NULL` |
| `WHERE` | attached to `MATCH`/`OPTIONAL MATCH`/`WITH` (pre-projection, or post-grouping when the clause aggregates) |
| `WITH` | projection, `DISTINCT`, implicit grouping keys, `WHERE`, `ORDER BY`; `WITH` after an updating clause starts a new reading scope |
| `RETURN` | projection, `DISTINCT`, `ORDER BY` (may only reference projected columns), `*` |
| `UNWIND` | expands a list; rebinding shadows (legal) |
| `ORDER BY` | `ASC`/`DESC`/`ASCENDING`/`DESCENDING`, multiple keys, `NULL` sorts last |
| `SKIP` / `LIMIT` | constant non-negative integer arguments (`NegativeIntegerArgument` / `InvalidArgumentType` otherwise) |
| `UNION [ALL]` | column-count checked (`DifferentColumnsInUnion`) |
| `CREATE` | nodes, relationships, multi-hop patterns; anonymous endpoints |
| `MERGE` | match-or-create of a whole pattern; `ON CREATE SET` / `ON MATCH SET` |
| `SET` | `n.p = v`, `n.p += v` (lists/maps), `n = {map}`, `n += {map}` (null values remove properties), `n:Label` |
| `REMOVE` | `n.p`, `n:Label` |
| `DELETE` / `DETACH DELETE` | expressions resolving to nodes/relationships |
| `CALL … YIELD` | not supported (see the certification report) |

Queries without a `RETURN` produce no result rows (updates report their
effect only through `RETURN`, per openCypher).

## Patterns

```
MATCH (a:Person {name: 'Ada'})-[:KNOWS]->(b)-[:KNOWS*1..3]->(c)
MATCH p = (a)-->(b)          ; p binds (:path (node rel node ...))
MATCH p = (a)-[:T*]->(b)     ; variable length, 1..∞
MATCH (a)-[:T*0..]->(b)      ; zero-length allowed
```

- Relationship patterns: `--`, `-->`, `<--`, `-[r:T]->`, `-[r:T {p: v}]->`,
  variable-length `*`, `*n`, `*n..m`, `*..m`, `*n..`.
- A variable-length relationship *variable* binds the **list** of traversed
  relationships.
- Path helpers: `length(p)`, `nodes(p)`, `relationships(p)`.

## Expressions

- **Literals**: integers (incl. octal `010`), floats (incl. `1e3`),
  strings (`'...'`/`"..."`), `true`, `false`, `null`, maps `{k: v}`, lists
  `[a, b]`, parameters `$name`.
- **Operators**: `+ - * / % ^`, comparison `= <> < > <= >=` (chained with
  implicit AND: `a < b < c` ≡ `a < b AND b < c`), `IS [NOT] NULL`,
  `IN`, `STARTS WITH`, `ENDS WITH`, `CONTAINS`, `NOT`, `AND`, `OR`, `XOR`.
- `+` on lists concatenates, and `list + element` appends / `element + list`
  prepends.
- **List comprehensions**: `[x IN list WHERE pred | expr]`.
- **Pattern comprehensions**: `[(a)-->(b) WHERE pred | b]`.
- **List predicates**: `all/any/none/single(x IN list WHERE pred)`.
- **Existential subqueries**: `EXISTS { pattern }`.
- **CASE**: simple (`CASE e WHEN ...`) and generic (`CASE WHEN ...`).
- **Map access**: `m.key`, `m['key']` (dynamic), `n.property`.
- Three-valued logic: `NULL` propagates; boolean operators reject
  non-boolean operands (`InvalidArgumentType`).

## Functions

- Aggregation: `count` (incl. `count(*)` and `count(DISTINCT x)`), `sum`,
  `avg`, `min`, `max`, `collect` (incl. `collect(DISTINCT x)`).  Aggregates
  may appear anywhere inside expressions
  (`1 + count(*)`, `head(collect(x))`, `size([x IN collect(r) WHERE ...])`);
  grouping keys are the non-aggregated projection items.
- Scalar: `coalesce`, `head`, `last`, `tail`, `size`, `length`, `type`,
  `labels`, `keys`, `properties`, `id`, `startNode`, `endNode`, `nodes`,
  `relationships`, `toBoolean`, `toInteger`, `toFloat`, `toString`, `abs`,
  `ceil`, `floor`, `round`, `sign`, `sqrt`, `toUpper`, `toLower`, `trim`,
  `lTrim`, `rTrim`, `reverse` (strings and lists), `replace`, `split`,
  `left`, `right`, `substring`, `range`, `rand`.
- Not supported: temporal constructors (`date`, `time`, `datetime`,
  `localtime`, `localdatetime`, `duration`) and their arithmetic — see the
  certification report; `percentileCont`/`percentileDisc`; `exists()` (use
  `EXISTS { }`); stored procedures.

## Errors

Errors are Common Lisp conditions under `SCALAXY:CYPHER-ERROR`, with
`CYPHER-ERROR-KIND` returning the openCypher subtype name (e.g.
`"UndefinedVariable"`).  Families: `SyntaxError`, `TypeError`,
`ArgumentError`, `EntityNotFound`, plus ~30 named subtypes
(`VariableAlreadyBound`, `VariableTypeConflict`, `ColumnNameConflict`,
`NoExpressionAlias`, `NestedAggregation`,
`AmbiguousAggregationExpression`, `InvalidArgumentValue`,
`MapElementAccessByNonString`, `NegativeIntegerArgument`, …).

## Storage

Graph records live in the replicated KV store under the key schemas
`n:<id>`, `r:<id>`, adjacency/type/label indexes, per-node id counters, and
blob spills (`b:<hash>`) for large binary properties — all pure views over
the append-only log (see the plan document, §8 and Appendix A).
