# Formal Cypher Specifications — Downloaded Sources

Sources of truth for the Scalaxy Cypher implementation and its certification.

## 1. openCypher 9 — Formal Specification (PDF)
- File: `specs/openCypher9.pdf` (96 pages, 895 KB)
- Source: https://s3.amazonaws.com/artifacts.opencypher.org/openCypher9.pdf
- Content: formal semantics of the Cypher query language: type system (the
  ANY/NULL/BOOLEAN/INTEGER/FLOAT/STRING/LIST/MAP/NODE/RELATIONSHIP/PATH
  lattice), expression semantics incl. three-valued logic and NULL
  propagation, clause semantics (MATCH/OPTIONAL MATCH/WHERE/RETURN/WITH/
  UNWIND/CREATE/MERGE/SET/REMOVE/DELETE/ORDER BY/SKIP/LIMIT/UNION), the
  aggregation semantics, function catalogue, and the error taxonomy
  (SyntaxError subtypes: InvalidArgumentType, VariableAlreadyBound,
  UndefinedVariable, VariableTypeConflict, InvalidNumberLiteral,
  IntegerOverflow, UnexpectedSyntax, InvalidAggregation,
  AmbiguousAggregationExpression, InvalidClauseComposition,
  NonConstantExpression, InvalidParameterUse, ...; TypeError subtypes;
  ArgumentError subtypes: NumberOutOfRange; EntityNotFound).
- Used for: semantic definition of our executor + reference evaluator, and
  the error-condition taxonomy in `src/cypher/conditions.lisp`.

## 2. openCypher TCK (Technology Compatibility Kit)
- Directory: `specs/openCypher/tck/features/` — 220 .feature files,
  **1,615 scenarios** covering clauses, expressions, and use cases.
- Source: https://github.com/opencypher/openCypher (shallow clone)
- Format: Gherkin (Cucumber). Step vocabulary observed (complete census):
  - Given an empty graph | Given any graph | Given the <name> graph
  - And having executed: <cypher>
  - And parameter values are: | table |  / And parameters are: | table |
  - When executing query: <cypher> | When executing control query: <cypher>
  - Then the result should be, in any order: | table |
  - Then the result should be, in order: | table |
  - Then the result should be (ignoring element order for lists): | table |
  - Then the result should be empty
  - And no side effects
  - And the side effects should be: | +nodes | +relationships | +labels |
    +properties | -nodes | ... | table |
  - Then a SyntaxError should be raised at compile time|runtime: <subtype>
  - Then a TypeError should be raised at runtime|any time: <subtype>
  - Then a ArgumentError should be raised at runtime: NumberOutOfRange
  - Then a EntityNotFound should be raised at runtime: DeletedEntityAccess
  - And there exists a procedure ... :: (...) — NOT in scope (no procedures)
- Result cells use Cypher literal syntax: `(:A {name: 'b'})`, `[1,2]`,
  `{k: 1}`, `null`, strings/numbers — parsed with our own expression parser
  (dogfooding; see tests/tck-runner.lisp).
- Tags: @skipGrammarCheck (22), @skipStyleCheck (11), @ignore (1).
- Predefined graphs: `tck/graphs/binary-tree-{1,2}/` with .cypher setup
  scripts and .json reference data.

## 3. openCypher Grammar (BNF)
- File: `specs/openCypher/grammar/openCypher.bnf`
- Used to cross-check the hand-written parser's productions.

## 4. GQL (ISO/IEC 39075)
- Not freely downloadable (ISO paywall). Our subset is chosen to be a
  GQL-aligned core of openCypher 9; see docs/cypher-implementation-plan.md.

## Certification methodology
1. Implement the reference evaluator directly from openCypher9.pdf sections
   (denotational semantics = the spec).
2. Differential tests: optimized executor ≡ reference evaluator.
3. Run the TCK with `tests/tck-runner.lisp` (Gherkin-subset parser + step
   handlers); report pass/fail/unsupported per scenario.
4. Emit `docs/cypher-certification.md`: per-scenario results, per-step
   coverage, error-taxonomy conformance, and the final conformance claim.
