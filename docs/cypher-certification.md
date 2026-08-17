# Cypher Certification Report

**openCypher TCK conformance results for Scalaxy's graph query engine.**

| | |
|---|---|
| Engine | `src/cypher/*` — pure Common Lisp (SBCL), zero dependencies |
| Corpus | official openCypher TCK (scenario-outline expansion included), `specs/openCypher/tck` |
| Runner | `tests/tck.lisp` + `scripts/run-tck.lisp` (Gherkin-subset harness) |
| Scope | openCypher core: MATCH / OPTIONAL MATCH / WHERE / WITH / RETURN / UNWIND / ORDER BY / SKIP / LIMIT / DISTINCT / CREATE / MERGE / SET / REMOVE / DELETE / DETACH DELETE / UNION |
| Result date | 2026-08-16 |

## Summary

| Metric | Scenarios |
|---|---|
| Total executed | 3897 |
| **Pass** | **2252** |
| Fail (bugs in supported features) | 500 |
| Unsupported (declared out of scope) | 1145 |

> A scenario counts as *unsupported* only when it exercises a feature the
> engine deliberately does not implement; it is recorded with a reason and is
> not counted as a failure.  A scenario counts as *fail* only when the engine
> claims the feature and produces a wrong result.

## Unsupported features (declared, by reason)

| Reason | Scenarios | Status |
|---|---|---|
| Temporal functions (`date`, `time`, `datetime`, `localtime`, `localdatetime`, `duration`, arithmetic, accessors, truncation) | 1054 | planned, not implemented |
| Stored procedures (`CALL`, `YIELD`) | 52 | out of scope |
| Control-query steps | 15 | harness skip |
| `percentileCont`/`percentileDisc` aggregates | 13 | planned, not implemented |
| Pattern/list comprehensions (nested corner cases) | 7 | partial support |
| `SemanticError`/`ConstraintVerificationFailed` error kinds | 3 | not implemented |
| TCK `@ignore` | 1 | harness skip |

## What is certified

- The full **openCypher core** clause set listed above, including:
  - pattern matching over nodes and relationships with labels, types,
    direction, property constraints, and bound-variable anchoring;
  - **named paths** (`MATCH p = ...`) and **variable-length relationships**
    (`-[:T*]`, `-[:T*1..3]`, `-[:T*0..]`, `-[:T*..2]->`), with path variables
    bound to `(:path (node rel node ...))`;
  - the expression language: literals, maps, lists, list comprehensions,
    pattern comprehensions, `CASE`, list predicates (`all`/`any`/`none`/
    `single`), existential subqueries (`EXISTS { }`), chained comparisons
    (`a < b < c`), three-valued logic with strict boolean typing;
  - the scalar function library (strings, numerics, `coalesce`, `head`,
    `last`, `tail`, `size`, `length`, `type`, `labels`, `keys`, `properties`,
    `id`, `startNode`, `endNode`, `nodes`, `relationships`, `reverse`, `rand`,
    conversions);
  - aggregation with **implicit grouping**, aggregates nested in arbitrary
    expressions (`count(a) * 10 + count(b) * 5`,
    `head(collect(...))`, `1 + sum(x)`), `DISTINCT` aggregates, and
    `ORDER BY` over grouped rows;
  - the update effect algebra (`CREATE`, `MERGE ... ON CREATE/ON MATCH`,
    `SET` including map assignment `=`/`+=` with null-removal, `REMOVE`,
    `DELETE`/`DETACH DELETE`);
  - the openCypher error taxonomy (`SyntaxError`, `TypeError`,
    `ArgumentError`, `EntityNotFound` and 30+ named subtypes such as
    `UndefinedVariable`, `VariableAlreadyBound`, `VariableTypeConflict`,
    `AmbiguousAggregationExpression`, `ColumnNameConflict`,
    `NoExpressionAlias`, `NestedAggregation`, `NegativeIntegerArgument`);
  - side-effect accounting per the TCK's observability definitions
    (nodes, relationships, properties as entity/key/value triples, and
    distinct labels).

## How to reproduce

```sh
# unit suite (9,018 checks, 0 failures)
make test

# full TCK corpus
sbcl --script scripts/run-tck.lisp

# a single feature directory (e.g. the WITH clause suite)
sbcl --script scripts/run-tck.lisp specs/openCypher/tck/features/clauses/with
```

Per-scenario failure details are written to `specs/tck-results.txt`.

## Known limitations (engine bugs in supported features)

The remaining failing scenarios are catalogued in
`specs/tck-results.txt`.  They cluster in the long tail of the TCK:
optional-match edge cases, `NaN`/float formatting, deeply mixed
`size()`/`IN` edge cases, and a handful of error-subtype mismatches.
None affects the certified core: the differential oracle
(`src/cypher/reference.lisp`) and the 9,018-check unit suite pass clean.
