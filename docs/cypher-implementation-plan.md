# Implementing Cypher in Scalaxy

## A Plan for the Graph Query Language in the Most Pure and Axiomatic Common Lisp

| | |
|---|---|
| Document | `docs/cypher-implementation-plan.md` |
| Project | Scalaxy 1.8.0 — distributed key/value database in Common Lisp (SBCL) |
| Scope | End-to-end analysis of the current codebase + full implementation plan for the openCypher query language |
| Status | Draft for review (Phase 0 deliverable) |
| Convention | "Cypher" below means the openCypher query language family (ISO GQL's precursor dialect) |

---

## 1. Purpose and Thesis

This document (a) analyzes the Scalaxy codebase end to end, and (b) specifies how to
add the Cypher graph query language to it in *the most pure and axiomatic Common Lisp
way*. That phrase is taken literally here; it is the design criterion, not a stylistic
flourish. It means:

1. **The semantics are stated first, as axioms and equations.** The property-graph model
   is defined as a 6-tuple with total-function laws; Cypher pattern matching is defined
   as *graph homomorphism* (the categorical definition, which coincides with openCypher's
   matching semantics); the query language is defined by a small *graph algebra* whose
   operators come with algebraic laws. Nothing is implemented that has not first been
   defined.

2. **The interpreter is the specification.** A metacircular *reference evaluator*
   (~100 lines, pure conses and hash tables) implements the denotational semantics
   directly. All optimized paths are *proven by testing* against this oracle
   (differential/law testing). This is the Lisp tradition of defining `eval` in terms
   of itself, applied to a query language.

3. **Every stage is a pure, total transformation between s-expressions.**
   `string -> tokens -> AST -> semantic AST -> logical plan -> physical plan -> result`
   is a chain of side-effect-free functions. I/O, the mutable store, and the network
   live only behind first-class reader/writer closures injected at the final stage
   (dependency injection as the mechanism of referential transparency). Queries are
   first-class data at every stage: any intermediate form can be printed, inspected,
   and re-evaluated (the code-as-data axiom).

4. **The existing system's invariants are inherited, not replaced.** Scalaxy already
   obeys three strong axioms: *log replay determinism* (store state = fold of the
   append-only log), *ring determinism* (key ownership = pure hash), and *record
   uniformity* (log record = wire record). The graph layer is defined **as a pure view
   over the existing KV store**: every index is a derived pure function of the log,
   every mutation flows through the existing replicated write path, and crash safety
   is inherited for free.

5. **Common Lisp's own machinery carries the design.** CLOS generic functions and
   multiple dispatch express the algebra over pluggable backends; conditions and
   restarts replace error strings; `deftype`/`check-type`/`assert` enforce the graph
   axioms at runtime in debug builds; macros compile constant queries at read time;
   the reader itself (via a scoped readtable) turns graph patterns into native data.
   The implementation remains dependency-free, matching the project's rules.

The result is not "a parser plus some joins" but a small, auditable *graph query
compiler* whose correctness rests on stated axioms, executable oracles, and law
tests — the closest thing to an axiomatic development that idiomatic Common Lisp
offers.

---

## 2. End-to-End Analysis of the Current Codebase

### 2.1 Module map

```
scalaxy.asd              ASDF definition, :serial t, 15 files, no dependencies
src/package.lisp         single package SCALAXY; shadows GET/DELETE; ~90 exports
src/util.lisp            string<->octets, hex-digest, FNV-1a-64, SplitMix64, hash-string
src/protocol.lisp        binary codec: opcodes 1..11, message plists, framing
src/storage.lisp         in-memory hash table + append-only length-prefixed log + replay
src/consistent-hash.lisp virtual-node ring (128 vnodes/node), binary-search lookup
src/replication.lisp     leader op log (bounded 1000 entries)
src/node.lisp            node struct: store + replicator + followers; node-dispatch
src/tcp.lisp             SBCL TCP server/client (sb-bsd-sockets, sb-thread)
src/json.lisp            dependency-free JSON encoder/decoder
src/http.lisp            minimal HTTP/1.1 server/client
src/web.lisp             web console: static assets, REST API, command console
src/gateway.lisp         ring routing over TCP, failover, scan fan-out, status aggregation
src/cluster.lisp         in-process cluster (tests): ring + synchronous replication
src/api.lisp             client API: connect/put/get/delete/scan over TCP
src/main.lisp            standalone node: CLI/env config, start-node, main
tests/run-tests.lisp     9,018 dependency-free checks (33 groups, incl. graph/Cypher), ~1.9s
web/                     console assets (index.html, app.js, app.css)
docs/                    protocol.md, rest-api.md
deploy/, scripts/, bin/  Docker/Kubernetes/compose, test launcher, node launcher
```

### 2.2 The four principal data flows

**Write path** (`api:put` -> durable, replicated):
`put` -> `tcp-request` (frame) -> `handle-connection` -> `node-dispatch` -> `node-put`
-> `store-put` (hash table + `store-log-mutation` + `finish-output`)
-> `node-replicate` (next-N ring followers, synchronous `+op-replicate+` acks).

**Read path** (`gateway-get`, with failover):
`%ring-owner-order` (owner first, then remaining members) -> TCP `+op-get+` to each
until one answers OK -> `store-get` (hash lookup).

**Scan path** (`gateway-scan`): fan out `+op-scan+` to *every* peer, merge, dedupe
(by key), sort by key, apply `limit`/`offset`. This is the model for label scans in
the graph layer.

**Web/API path**: `http-serve` -> `%web-dispatch` (method/path routing over
`/healthz`, `/api/status`, `/api/keys...`, `/api/query`, `/api/node-status`) ->
`run-command` for the console (`put/get/delete/scan`), JSON everywhere.

### 2.3 Invariants the codebase already obeys (its implicit axioms)

- **A0-log:** the store table equals the fold of the append-only log; replay on open
  reconstructs state exactly (`store-replay` / `store-apply-log-record`).
- **A0-wire:** log record format = network frame body format; a replicated record is
  byte-identical to the logged record (`frame-message` used by both).
- **A0-ring:** the owner of a key is a pure deterministic function of the key
  (`ring-lookup` = `hash-string` + binary search); all nodes compute the same ring.
- **A0-replication:** writes are acknowledged only after followers acknowledge;
  reads fail over to replica holders (synchronous, at-least-once).

These four axioms are load-bearing for the graph layer and are **never bypassed**:
the graph layer only ever calls `store-put`/`store-delete`/`node-put`/`node-dispatch`
and their gateway analogues.

### 2.4 Constraints that shape the design

1. **Zero dependencies** (CONTRIBUTING): lexer, parser, and evaluator must be
   hand-written — which is exactly where Lisp-native designs (recursive descent over
   a hand-rolled scanner, s-expression ASTs) are at their strongest.
2. **ANSI Common Lisp core**, SBCL-only code behind `#+sbcl` and confined to TCP/HTTP.
3. **Values are octet vectors**; the web layer converts to utf8/hex for display.
4. **Message protocol is plist-based** (`:op`, `:key`, `:value`, `:seq`, `:status`,
   `:pairs`), opcodes 1..11 are taken; **12+ are free**.
5. **`store-scan` is prefix-based only** — the sole existing index primitive.
6. **Style**: 2-space indentation, docstrings on all public functions, `+constants+`,
   `defstruct` accessors, plists over structs for messages.

### 2.5 Extension points where Cypher plugs in

| Layer | File | Change |
|---|---|---|
| Data model + encoding | new `src/graph.lisp` | entities, records, derived indexes, invariants |
| Compiler | new `src/cypher/*.lisp` | lexer, parser, semantics, algebra, planner, executor, reference |
| Protocol | `src/protocol.lisp` | opcodes 12..16 + payload layouts |
| Node dispatch | `src/node.lisp` | new `case` arms for graph opcodes |
| Gateway | `src/gateway.lisp` | Cypher entry point, primitive fan-out, routing |
| Cluster | `src/cluster.lisp` | in-process `cluster-cypher` for tests |
| Client API | `src/api.lisp` | `cypher`, `graph-*` primitives |
| Web | `src/web.lisp`, `web/` | `POST /api/cypher`, `cypher` console command, result rendering |
| Packaging | `src/package.lisp`, `scalaxy.asd`, `tests/` | exports, serial modules, new test groups |

---

## 3. Design Thesis: What "Pure and Axiomatic Common Lisp" Buys Here

### 3.1 The four purity principles

- **P1 (denotational first).** Every language construct gets an equation
  `[[construct]] : inputs -> outputs` before any code is written (§4, §5.2).
- **P2 (pure pipeline).** Lexing through planning are total pure functions; the only
  impure stage is the *cursor pump* that calls injected reader/effect closures (§6, §7).
- **P3 (oracle-driven).** The reference evaluator defines correctness; the optimized
  executor is accepted only when differential law tests pass (§7.4, §12).
- **P4 (axioms are artifacts).** Every axiom has a concrete home — a docstring, an
  `assert`, a law test, or the reference implementation itself — and the mapping is
  tabulated (§12.1).

### 3.2 Lisp mechanisms mapped to design needs

| Need | Lisp mechanism | Where |
|---|---|---|
| Queries as data at every stage | s-expression AST/plans, `print`/`read` round-trips | §6.2 |
| Pattern syntax as native data | scoped readtable (`copy-readtable` + macro chars for `->`, `<-`, `[ ]`) | §6.3 |
| Constant queries compiled once | `defcypher` macro (parse + plan at macroexpansion time) | §6.5 |
| Open operator set over many backends | CLOS generic functions + multiple dispatch (`memory-store`, `tcp-store`, `gateway-store`) | §7.3 |
| Error handling as protocol | `define-condition` hierarchy + restarts (`abort-query`, `use-value`, `skip-clause`) | §7.6 |
| Axiom enforcement | `deftype`, `check-type`, `assert` (debug builds), invariant probes | §4, §12 |
| Bag/table iteration | pull-based cursor closures (no external `series`/`iterate` dependency) | §7.1 |
| The spec itself | the metacircular reference evaluator | §7.4 |

### 3.3 Layering principle

```
                   Cypher string  |  Lisp pattern forms (readtable)
                                   |
        +--------------------------+--------------------------+
        |  pure  |  lexer -> parser -> AST                     |
        |        |  semantics (scopes, types, constant fold)   |
        |        |  planner (AST -> algebra plan -> cursors)   |
        |        |  executor = [[plan]] applied to injected    |
        |        |  graph-reader/graph-effect closures         |
        +------------------------------------------------------+
                   | graph-reader = graph operations over KV
                   v
        +--------------------------+--------------------------+
        |  impure|  store (hash + log), node-dispatch,         |
        |  (edge)|  TCP/HTTP servers, ring, replication        |
        +--------------------------+--------------------------+
```

The graph layer is *defined over* the KV layer (a view with derived indexes);
the Cypher layer is *defined over* the graph layer (a pure evaluator over
reader closures). Each boundary is a stated abstraction with laws (§4).

---

## 4. Axiomatic Specification

### 4.1 Group A — the property-graph model

**A1.** A property graph is a 6-tuple `G = (N, R, src, dst, lab, prop)`:

- `N` — finite set of nodes; `R` — finite set of relationships;
- `src : R -> N`, `dst : R -> N` — **total** functions (every relationship has
  exactly one start node and one end node);
- `lab : (N ∪ R) -> P(Labels)` — label map; a node carries zero or more labels,
  a relationship carries **exactly one** label (its *type*): `|lab(r)| = 1`;
- `prop : ((N ∪ R) × Keys) -|-> Values` — **partial** property map; values range
  over the Cypher value lattice (§4.6).

**A2.** Element identity: every element has an immutable, globally unique id
(its *oid*), which is exactly its storage key (§8). Identity equality is oid
equality; property equality is irrelevant to identity.

**A3.** The label and property maps are the *only* content: two graphs are equal
iff the 6-tuples are equal. Any implementation state beyond the 6-tuple is either
an index (§8.3, derivable) or a cache (droppable).

Enforcement: `check-type`/`assert` in `src/graph.lisp` (`graph-check-invariants`),
invariant law tests (T1 in §12).

### 4.2 Group B — pattern matching as homomorphism

**B1.** A *pattern* `P` is a property graph whose elements are either constants
(bound to concrete graph elements by oid) or variables (from a finite set `Var`).

**B2 (homomorphism semantics).** The match set of `P` in `G` is

```
Match(P, G) = { h : P -> G  |  h a graph homomorphism }
```

i.e. all maps `h` with:

- `h(src_P(r)) = src_G(h(r))`, `h(dst_P(r)) = dst_G(h(r))`   (structure preserved)
- `lab_P(x) ⊆ lab_G(h(x))`                                     (labels preserved)
- for each property literal `(x, k, v)` in `P`: `prop_G(h(x), k) = v`  (props preserved)
- constants in `P` map to themselves.

**B3.** `MATCH` returns the bag of all such homomorphisms projected to the query's
variables — *homomorphisms, not embeddings*: distinct pattern elements may map to
the same graph element, and no implicit all-different constraint applies
(this is exactly openCypher's semantics; the common "no node twice" intuition is
not part of the definition).

**B4 (decomposition).** A pattern is a *cospan composition*: joining pattern
fragments along shared variables is composition in the category of relations, and
the match bag of the whole pattern is the relational join of the fragment match
bags (law L11). This is what licenses any join-order rewrite and any distributed
decomposition (§10).

The reference evaluator enumerates `Match(P, G)` literally (naive, exponential,
correct); the optimized executor computes the same bag by index joins. B4 plus
the law tests make the two provably interchangeable in practice (§7.4, §12.2).

### 4.3 Group C — the query algebra (operators and laws)

**C1 (binding tables).** A *binding* is a partial function `Var -|-> (N ∪ R ∪
Values)`. A *table* is a **multiset** (bag) of bindings. All read operators are
functions `Table -> Table` (unary) or `Table × Table -> Table` (binary); bags
carry multiplicity, and duplicate rows are preserved unless an explicit
`DISTINCT`/aggregation collapses them.

**C2 (operator set).** The algebra is deliberately small (each operator gets a
denotational equation in §5.2):

```
scan-nodes(labels, props) -> Table        leaf: nodes satisfying label+property predicates
scan-rels(type, props)    -> Table        leaf: relationships satisfying predicates
expand(T, v, rv, dir, types) -> Table     T extended with (rel, neighbor) pairs
filter(T, p)              -> Table        p is a pure predicate; T kept where p = true
project(T, vars)          -> Table        restrict bindings (multiplicity kept)
distinct(T)               -> Table        bag -> set
union-all(T1, T2)         -> Table        additive bag union
optional(T1, T2, keys)    -> Table        left outer join on keys
group-aggregate(T, keys, aggs) -> Table   grouping (WITH/RETURN aggregation)
sort(T, spec)             -> Table        total, stable order
skip(T, k), limit(T, k)   -> Table        windowing
unwind(T, list-expr, v)   -> Table        one row per list element
```

**C3 (the effect algebra).** Writes are *pure state transformers* evaluated only
by the transaction executor: `create(P, β) : Graph -> Graph`, `merge(P, β)`,
`delete(ids)`, `detach-delete(ids)`, `set-props(ids, kvs)`, `remove-props(ids, keys)`,
`set-labels/remove-labels`. Each is specified by its pre/post-state equation
(§5.2), and `merge` satisfies idempotence: `merge;merge = merge` (law L15).

**C4 (laws).** The algebra obeys identities that double as rewrite rules and as
tests (§12.1, Appendix B): filter distributes over union; project/filter commute
on variable-visible predicates; join is associative and commutative (bags);
`distinct` is idempotent; skip+limit fuse; filter pushdown across expand is
licensed by B4; the expand operator *is* the pullback of the adjacency relation.

### 4.4 Group D — storage embedding axioms (graph as a view over KV)

**D1 (facts only).** The log stores only element facts (node records, relationship
records, deletions). Graph state is the fold of the log: `G = γ(fold(log))`, with
`γ` the decoding of §8. Indexes are **not** logged in v1.

**D2 (derivability).** Every index is a pure function of the store:
`I = δ(store)`. Indexes are rebuilt by re-running `δ` after log replay
(`graph-rebuild-indexes`). Corollary: an index can never be lost or corrupted in
a way that survives restart — crash safety is inherited from A0-log.

**D3 (routing compatibility).** The record key of element `x` hashes through the
existing ring (`ring-lookup`); all facts about `x` (its record, its adjacency
entries, its label-index entries) live on `x`'s ring owner. Single-hop expansion
therefore costs one routed access to one node; ownership of adjacency data never
straddles shards.

**D4 (no bypass).** The graph layer mutates only via `store-put`/`store-delete`
(or `node-put`/gateway equivalents), so durability, replication, and failover
semantics are exactly the existing ones. There is no second write path.

### 4.5 Group E — purity axioms of the language layers

- **E1:** lexer, parser, semantic analyzer, and planner are *total pure functions*:
  `(query-string, schema-stats) -> plan` or a condition (§7.6).
- **E2:** the executor is pure *given* its injected graph-reader closures; all
  I/O occurs inside those closures. Result: the same plan runs identically on a
  memory store, a TCP store, or a gateway store — the backends differ only in the
  closures (multiple dispatch, §7.3).
- **E3 (oracle):** the reference evaluator (§7.4) defines `[[·]]`; every optimized
  execution must agree with it (differential tests, §12.2).
- **E4 (data):** AST, plan, and result are s-expressions; `print`/`read` round-trip
  (law L14), and any stage can be inspected interactively at the REPL.

### 4.6 The Cypher value lattice in Common Lisp

| Cypher type | CL representation (reference path) | Notes |
|---|---|---|
| NULL | a singleton object `+cypher-null+` (e.g. a fresh `gensym`-based sentinel or a dedicated struct) | **Deliberate purity point:** CL's `NIL` conflates false/empty/absent; Cypher separates NULL, FALSE, and empty. `NULL` is never `NIL`. |
| BOOLEAN | `t` / `nil` | |
| INTEGER | CL integer | |
| FLOAT | CL float | |
| STRING | CL string | |
| LIST | CL list | |
| MAP | alist of `(key . value)` in the reference path; hash table in the optimized path | law L-map: both agree |
| NODE / RELATIONSHIP | structs carrying oid + labels + props | |
| PATH | list of alternating node/rel/node | with the no-repeat-relationship axiom for variable-length matching |
| ANY | any of the above | dynamic typing via `typecase` dispatch |

Three-valued logic follows openCypher exactly (Kleene tables), stated as
equations in §5.3: `NULL AND false = false`, `NULL AND true = NULL`,
`NULL OR true = true`, `NOT NULL = NULL`; `WHERE` keeps exactly the rows whose
predicate evaluates to TRUE; `= NULL` is NULL (use `IS NULL`).

---

## 5. Language Scope: The Cypher Subset and Its Semantics

### 5.1 Scope decision

Implement **openCypher core**, pinned by the openCypher specification and TCK
scenarios, in three capability tiers:

- **Tier 1 (pure, read-only):** `MATCH`, `OPTIONAL MATCH`, `WHERE`, `WITH`,
  `RETURN`, `UNWIND`, `ORDER BY`, `SKIP`, `LIMIT`, `DISTINCT`; full expression
  language (§5.3); scalar and aggregate functions (§5.4).
- **Tier 2 (effects):** `CREATE`, `MERGE` (single connected pattern), `SET`,
  `REMOVE`, `DELETE`, `DETACH DELETE` — the effect algebra C3.
- **Tier 3 (extension):** variable-length relationships `-[:T*1..3]->`,
  `shortestPath` (BFS), pattern comprehensions, list comprehensions,
  `CASE WHEN` in updates, schema commands (`CREATE CONSTRAINT ... UNIQUE`)
  — after Tiers 1–2 are law-tested.

### 5.2 Grammar (EBNF sketch) and denotational equations

```
Query        := SingleQuery | UnionQuery ;
UnionQuery   := SingleQuery { UNION [ALL] SingleQuery } ;        -- additive bag union (C2)
SingleQuery  := ReadingClause* Return | ReadingClause* UpdatingClause+ [Return] ;
ReadingClause:= MATCH Pattern [WHERE Pred] | OPTIONAL MATCH Pattern [WHERE Pred]
              | UNWIND Expr AS Var | WITH ProjectionBody [WHERE Pred] ;
UpdatingClause := CREATE Pattern | MERGE Pattern | SET SetItem{,SetItem}
                | REMOVE RemItem{,RemItem} | DELETE Expr{,Expr} | DETACH DELETE Expr{,Expr} ;
Pattern      := PatternElement { , PatternElement } ;            -- fragments joined on shared vars (B4)
PatternElement := NodePattern { PatternPartChain | PatternPartChain? } ;
NodePattern  := ( Var ) [ :Label {| :Label} ] [ {PropMap} ] ;
PatternPartChain := RelationshipPattern NodePattern ;
RelationshipPattern := <-- | --> | -[Var?[:Type]{Props}]-  (directed / undirected) ;
ProjectionBody := DISTINCT? ProjectionItem{,ProjectionItem}
                | * | DISTINCT?
                | GROUP BY? aggregate positions ;
ProjectionItem := Expr [AS Var] ;
SetItem := Var.Prop = Expr | Var = Expr | Var += Expr | Var:Label ;
RemItem := Var.Prop | Var:Label ;
Expr  := literal | parameter | Var | Var.Prop | [Expr{,Expr}] | {key:Expr{,key:Expr}}
       | Expr op Expr | unop Expr | function(Expr{,Expr}) | CASE WHEN ... END
       | all/any/none/single(var IN Expr WHERE Pred) | pattern comprehension (T3) ;
```

Denotational equations (selections; the full set lives with the implementation):

```
[[MATCH P]] T            = T ⋈κ Match(P, G)          -- κ = shared variables; join per B4
[[OPTIONAL MATCH P]] T   = T ⟕κ Match(P, G)          -- unmatched rows get NULL bindings
[[WHERE p]] T            = filter(T, p)               -- rows where p = true
[[UNWIND e AS v]] T      = for each row β, one row β' per element of [[e]]β (β' = β ∪ {v ↦ elem})
[[RETURN e1..en]] T      = project-rename over bag T; aggregates make it group-aggregate(C2)
[[DISTINCT]]             = distinct(C2)
[[ORDER BY s]] T         = stable sort by spec s
[[SKIP k]][[LIMIT l]]    = skip(T,k) ∘ limit(·,l)      -- law L5 fusion
[[CREATE P]] T           = T' where G' = G + fresh elements for each P element per row
                           (src/dst totality axiom A1 enforced: rels only with both endpoints)
[[MERGE P]] T            = if Match(P,G) ≠ ∅ for the row's constants: bind; else create whole P
                           (law: merge;merge = merge)
[[DELETE xs]] / [[DETACH DELETE xs]]
                         = remove elements; DETACH first removes incident rels
                           (A1 totality: deleting a node deletes/forbids dangling rels)
[[SET x.p = e]]          = prop' = prop ∪ {(x,p,val)}; [[REMOVE x.p]] = prop minus the pair
[[SET x :L]]             = lab' = lab ∪ {L};   [[REMOVE x:L]] = lab minus L
```

Grouping axiom (L10): in `WITH`/`RETURN`, variables not wrapped in aggregates are
grouping keys; `count(*)` counts rows; aggregation over an empty table yields one
row with `count = 0` and other aggregates NULL.

### 5.3 Expressions

Literals (int, float, string `'...'`/`"..."`, `true/false`, `null`, lists, maps),
parameters `$p`, property access `x.p` (missing property = NULL — a consequence
of A1's partial map), operators `+ - * / % ^` (Cypher's numeric coercion and
string `+` concat rules), comparison operators with the Cypher ordering rules,
`IS NULL`/`IS NOT NULL`, `IN` (list membership with NULL propagation), `STARTS
WITH`/`ENDS WITH`/`CONTAINS`, `AND OR XOR NOT` under the Kleene tables, list
indexing `l[i]`, map access `m[k]`, `CASE WHEN`. Predicates: `all`, `any`,
`none`, `single`, `exists()`.

The expression module is a small pure interpreter: `eval-expr(expr, binding,
graph-reader) -> value`; constant folding at plan time for expressions over
literals only.

### 5.4 Functions

Scalar: `coalesce, head, last, size, length, toBoolean, toInteger, toFloat,
toString, type, labels, keys, properties, id, nodes, relationships, startNode,
endNode, abs, ceil, floor, round, sign, sqrt, toUpper, toLower, trim, lTrim,
rTrim, replace, split, left, right, substring, range, reverse, tail`.
Aggregates (only legal in WITH/RETURN positions): `count, sum, avg, min, max,
collect`. `id(x)` maps an oid string to `fnv1a-64(oid)` — stable, matches the
project's existing hash utilities.


---

## 6. Architecture: The Compiler Pipeline

### 6.1 Stage diagram

```
Cypher string                          Lisp pattern forms (readtable, §6.3)
      |                                        |
      v                                        v
  LEXER  (pure: string -> token list)  ......+  both produce the SAME s-expressions
      v
  PARSER (pure recursive descent: tokens -> AST)
      v
  SEMANTIC ANALYSIS (pure: AST -> annotated AST)
        - scope resolution (WITH scopes, subquery scopes)
        - variable-usage checks -> CYPHER-SEMANTIC-ERROR conditions
        - type inference (light) + constant folding
        - rewrite sugar (map/property patterns) to core
      v
  PLANNER (pure: annotated AST -> algebra plan)
        - pattern -> operator DAG via B4 decomposition (join order by selectivity)
        - index selection (label index, adjacency, degree stats — all derived data)
        - filter pushdown (licensed by laws C4/L1-L3)
        - subquery / WITH pipeline splitting
      v
  EXECUTOR (plan -> cursor of result rows)      [pure modulo injected closures, E2]
      v
  RESULT SERIALIZATION (JSON table for wire/web)
```

### 6.2 The AST is s-expressions

```
(:query (:match (:pattern (:node (:var ?a) (:labels (:user))
                         (:props ((:name "Ada"))))
                  (:rel (:var ?r) (:types (:KNOWS)))
                  (:node (:var ?b))))
        (:where (:call := (:prop ?b :age) (:int 42)))
        (:return (:item (:var ?a)) (:item (:prop ?b :name) :as "n")))
```

Everything downstream — scope maps, plans, cursor graphs — is likewise plain
data (`plist`-keyed or `defstruct`). Consequences: plans are diffable in tests,
dumpable in logs, and `(eval plan-as-code)` is possible (the executor is close
enough to an interpreter that the plan itself can be *compiled to a closure* —
constant queries become code at compile time, §6.5).

### 6.3 Reader integration: patterns as native Lisp data

A scoped readtable (`copy-readtable`) teaches the reader graph syntax:

```lisp
(with-cypher-syntax
  (match (?a)-[:KNOWS {since: 2020}]->(?b)
    (where (> (?b.age) 30))
    (return ?a ?b.name)))
```

- `?a` already reads as the symbol `?A` in standard CL — variables are symbols
  whose names start with `?`, **free of charge**;
- `[` becomes a macro character that collects a list to `]`;
- `->`, `<-`, `-` become dispatch characters producing direction tokens;
- `{k: v}` reads via `{`/`}` macro characters into an alist (property maps are
  alists — the reference representation, §4.6).

The string front-end (standard Cypher) and the reader front-end compile to the
**identical** AST — a syntactic purity law ("two syntaxes, one semantics"),
tested as L14'.

### 6.4 Parser design

- Hand-rolled scanner (~250 lines): keyword/identifier rules, string escapes
  (single- and double-quoted), numbers, parameters `$name`, comments `//` and
  `/* */`, operators as maximal-munch tokens.
- Recursive-descent parser (~600 lines) with one function per grammar production,
  precedence-climbing for expressions, and precise positions on every node for
  error reporting. No parser generators — consistent with the dependency rule
  and with the project's minimal-dependency ethos.
- Errors: `signal 'cypher-parse-error :query query :position pos :expected ...`.

### 6.5 Name resolution, analysis, and compile-time queries

- Scopes: `MATCH` introduces variables; `WITH` closes a scope (unmentioned
  variables do not pass — a semantic check); `OPTIONAL MATCH` may introduce
  fresh variables; `UNWIND` binds one; aggregations resolve at `WITH`/`RETURN`.
- Unbound variable, undefined function, aggregation misuse, and misplaced
  update-clause checks all signal `cypher-semantic-error`.
- Type inference is deliberately light (literal folding + nullable/definite
  tracking); the language is dynamically typed by design (ANY lattice, §4.6).
- `defcypher` macro: `(defcypher get-people "MATCH (p:Person) WHERE p.age > $min
  RETURN p")` parses, analyzes, and plans **at macroexpansion time** and expands
  to a closure `(lambda (graph params) ...)` — constant queries pay compile cost
  once. This is the compile-time-computation axiom made user-visible.

### 6.6 Planner

- **Pattern decomposition (B4):** the pattern graph is split into its connected
  fragments; within a fragment, a spanning join tree is chosen: most selective
  leaf first (label + property constant -> label index scan; bound variable ->
  point get; otherwise label scan), then edges expand outward via adjacency.
  Selectivity uses *derived* statistics (index sizes, per-type degree counts —
  themselves pure functions of the store, rebuilt with the indexes).
- **Filter pushdown** across joins/expands is a direct application of laws
  L1–L3; the planner emits the pushed-down form and the law tests verify
  equivalence.
- **Physical plans** select among: point get, prefix scan, adjacency expand,
  hash join (shared variable), nested-loop join (small side), and
  sort/hash-based grouping. The plan is a DAG of operator descriptors; the
  executor turns it into pull cursors (§7).

---

## 7. Execution Engine

### 7.1 Pull-based cursors

Every operator compiles to a **cursor**: a closure-plus-state object with

```lisp
(defgeneric cursor-next (cursor)  ; -> row | :eof
  (:documentation "Pull the next row from CURSOR, or :EOF at exhaustion."))
```

Pull semantics give laziness for free (correct `LIMIT` short-circuiting,
bounded memory on `UNWIND` of large lists), stack safety (iterative loops, no
deep recursion over results), and natural composability (a cursor graph is a
DAG of closures — classic Lisp). Rows are bindings (§7.2); aggregation cursors
buffer only their group state; `ORDER BY` materializes + sorts stably.

### 7.2 Bindings

- Reference path: **alists** — `((?a . "g0-7") (?r . "g1-3"))`, purely
  functional, no mutation; law-grade clarity.
- Optimized path: an *environment* of columns (vector of values + variable ->
  slot map), mutated in place by cursors for speed. Both must agree
  (differential tests). The optimized path is free to mutate its own scratch
  state: purity is required at the module boundary (E2), not inside a cursor's
  private registers — the plan states this explicitly so reviewers can audit
  it.

### 7.3 Operators as generic functions over backends

```lisp
(defgeneric graph-get-element (graph oid))                 ; memory | tcp | gateway
(defgeneric graph-scan-nodes (graph label prop-preds))
(defgeneric graph-expand (graph oid direction types))
(defgeneric graph-mutate (graph effects)                   ; only the effect executor calls it
```

The algebra operators (`scan-nodes`, `expand`, ...) are *backend-agnostic* and
are implemented once against this protocol. Backends:

- `memory-graph` — wraps a local `store` (tests, single node);
- `tcp-graph` — one `tcp-request` per opcode over the data plane;
- `gateway-graph` — routes to ring owners with failover (reuses
  `%ring-owner-order` and `gateway-request` verbatim).

Adding a backend = defining methods; the engine is untouched. This is the
CLOS open-set axiom applied to the storage layer.

### 7.4 The metacircular reference evaluator

`src/cypher/reference.lisp` implements `[[·]]` *literally* from §4–§5:

- pattern matching enumerates hom-set candidates over the pattern's variables
  (nested loops over candidate domains, pruning by the B2 conditions) — the
  exponential algorithm, used only as the oracle;
- tables are lists of alists (bags = plain lists, multiplicity preserved);
- joins are nested loops; no indexes.

Roughly 100 lines. Its roles: (a) executable specification, (b) differential
oracle for the optimized executor, (c) a teaching artifact, (d) a debugging
tool (`(reference-eval query-string)` at the REPL). This is the Lisp
"interpreter as definition" tradition made literal.

### 7.5 The effect algebra executor (updates)

Tier 2 clauses compile to *effect lists* — pure descriptions
(`(:create (:node ...))`, `(:set-prop oid k v)`, `(:delete oid)`, ...).
The transaction executor:

1. validates the effect list against A1 (no dangling relationships, no
   relationship without both endpoints) — `graph-check-invariants`;
2. resolves fresh oids (shard-local minting, §8.4);
3. applies effects through `graph-mutate` (which lowers to `store-put` /
   `store-delete` sequences — D4), updating derived indexes in the same
   critical section;
4. reports created/deleted counts and returns the updated bindings.

`MERGE` runs a match against the pattern; on empty result, it creates the
whole pattern (atomicity at the single-node level; distributed MERGE is a
documented v1 limitation, §14).

### 7.6 Error handling: conditions, not strings

```lisp
(define-condition cypher-error (error)
  ((query :initarg :query :reader cypher-error-query)))
(define-condition cypher-parse-error    (cypher-error) ((position ...)))
(define-condition cypher-semantic-error (cypher-error) ((form ...)))
(define-condition cypher-type-error     (cypher-error) ((expected ...) (actual ...)))
(define-condition cypher-constraint-violation (cypher-error) ((axiom ...)))
```

Restarts: `abort-query` (return an error table to the caller), `skip-clause`
(development aid), `use-value` (REPL interactivity). The web layer maps
conditions to HTTP 400/500 JSON; the console prints a `report`. This replaces
the ad-hoc string error style with the CL condition protocol — the idiomatic
"errors as a protocol" purity point.

---

## 8. Graph Storage: A Pure View Over the KV Store

### 8.1 Key schemas (Appendix A has the full table)

```
n:<eid>                      node record          (octets: encoded labels+props)
r:<eid>                      relationship record  (octets: type,start,end,props)
nl:<label>:<eid>             ""  (presence)       node label index
e:<eid>:o:<type>:<rid>       ""  (presence)       out-adjacency of node <eid>
e:<eid>:i:<type>:<rid>       ""  (presence)       in-adjacency of node <eid>
g:next:<node-id>             u64 counter          shard-local oid minting (best effort)
```

- `<eid>` = `"<node-id>-<local-seq>"`, e.g. `"g0-7"`: uniqueness follows from
  node identity, so minting needs **no global coordination** (a documented
  axiom; §8.4).
- All element keys are strings, so they route through the existing ring,
  replicate, log, and prefix-scan exactly like today's keys (D3, D4).
- `DETACH DELETE` of a node needs all incident relationships; both adjacency
  directions are stored under the node's own `<eid>` (out under `o`, in under
  `i`), so deletion = prefix scans of `e:<eid>:o:` and `e:<eid>:i:` — local to
  the node's shard.

### 8.2 Record encoding

Values in the store are octets; graph records are UTF-8 JSON built with the
**existing** `json.lisp`:

- node record: `{"labels":["Person"],"props":{"name":"Ada"}}`
- relationship record: `{"type":"KNOWS","start":"g0-7","end":"g1-3","props":{"since":2020}}`

Choosing JSON reuses a battle-tested in-tree codec, keeps records visible in
the web console's data browser (they are ordinary values), and honors the
dependency rule. A binary sexp/record codec is listed as an optional later
optimization (same schema, different codec — a codec-independence axiom,
analogous to A0-wire).

### 8.3 Derived indexes and rebuild

Label and adjacency indexes are **in-memory derived data** (hash tables) with
`I = δ(store)`; `graph-rebuild-indexes` re-derives them from the replayed
store at startup (prefix scan of `n:`/`r:` + fold). Replication is unaffected:
indexes are never replicated because they are not facts; a replica rebuilds
its own. Statistics used by the planner (index sizes, degree counts) are
derived the same way — one rebuild, three consumers.

*Phase-3 option:* log redundant index records (idempotent apply) to remove
startup rebuild cost; correctness still rests on D2, so a lost index record
is harmless. This is a performance decision, not a correctness one.

### 8.4 Id minting

`g:next:<node-id>` is a per-node counter. Read-increment-write is safe on a
single node (node dispatch is serial per connection but concurrent across
connections — a per-node lock suffices; noted in §14). The existing protocol
has no atomic compare-and-swap; a `+op-cas+` opcode (16) is proposed in §9.1
for correctness under concurrency and for `MERGE` idempotence (Tier 2).

### 8.5 Mutation path

`graph-mutate` lowers every effect to a sequence of `store-put`/`store-delete`
calls (record keys + index keys), which appends to the log and replicates
(D4). Batch atomicity is single-node: a crash mid-sequence is healed by replay
+ index rebuild (D1/D2), because the log contains facts only.

---

## 9. Protocol, Node, Gateway, Client, and Web Integration

### 9.1 New opcodes (12+, continuing the existing numbering)

| Opcode | Name | Request payload | Reply |
|---|---|---|---|
| 12 | `+op-cypher+` | query string + params (length-prefixed `(k v)` pairs) | `RESPONSE` whose `:value` is a JSON-encoded result table |
| 13 | `+op-graph-scan+` | scan descriptor (kind u8: 1=label-nodes, 2=rels-by-type) + label/type string + property predicates (pairs) | `RESPONSE :pairs` of `(key . record)` |
| 14 | `+op-graph-expand+` | node oid string + direction u8 (0=out,1=in,2=both) + type list (strings) | `RESPONSE :pairs` of `(rel-record . neighbor-record)` |
| 15 | `+op-mget+` | count u32 + key strings | `RESPONSE :pairs` (bulk fetch after adjacency scans) |
| 16 | `+op-cas+` | key + expected-octets-or-empty + new octets | `ACK` ok/conflict (Tier 2: `MERGE`, counters) |

The result-table JSON for `+op-cypher+` uses tagged objects:
`{"~t":"node","id":"g0-7","labels":[...],"props":{...}}`,
`{"~t":"rel",...}`, `{"~t":"path",...}`; scalars map 1:1 to JSON (existing
codec). This keeps the web layer's JSON plumbing unchanged.

### 9.2 Node dispatch

New `case` arms in `node-dispatch` delegate to the graph layer; writes go
through the graph-mutation path so replication continues to happen exactly as
today. No change to `tcp.lisp` beyond nothing — it is opcode-agnostic.

### 9.3 Gateway and cluster

- `gateway-cypher (gateway query &key params)`: the executor runs on the
  coordinating node, but all graph reads go through the `gateway-graph`
  backend — each primitive op (`+op-graph-scan+`, `+op-graph-expand+`,
  `+op-mget+`) is routed to the ring owner(s) with the existing failover
  order; Tier 3 additionally ships whole (sub)queries via `+op-cypher+` and
  merges tables.
- `cluster-cypher` mirrors this in-process for tests (like `cluster-scan`).

### 9.4 Client API

```lisp
(cypher client "MATCH (a)-[:KNOWS]->(b) WHERE a.name = $n RETURN b" :params '(("n" . "Ada")))
;; -> rows as plists/alists
```

plus `graph-put-*`-style primitives if direct element access is wanted.
All through `tcp-request`, same as every existing call.

### 9.5 Web console and REST

- `POST /api/cypher` `{"query":"...","params":{...}}` -> JSON result table
  (`{"columns":[...],"rows":[[...]],"count":N,"timeMs":...}`).
- Console command `cypher <query>` (multi-line input already supported by the
  web console: Enter runs, Shift+Enter newlines) routed through `run-command`.
- Data browser: graph records decode to readable JSON previews automatically
  (they are ordinary values).
- `web/index.html` + `app.js`: a "Graph" tab with a query box, result grid,
  and error panel (Tier 3 polish; command-console support lands in Tier 1).

---

## 10. Distributed Execution (Phased)

**Phase A (Tier 1/2): coordinator + primitive routing.** The planner splits a
pattern into operators; each leaf/expand is one routed RPC (label scan on all
nodes → merge/dedupe, like `gateway-scan`; expand → point request to the
node-oid's owner). The coordinator (any node's gateway) composes results with
hash/nested-loop joins. Justification: B4 (join associativity) makes any
decomposition semantically safe; D3 makes each expand a single-shard access;
C4's filter-pushdown laws let the coordinator pre-filter bindings before the
next RPC round, shrinking shipped tables.

**Phase B (Tier 3): query shipping.** Whole (sub)patterns ship via
`+op-cypher+` to each shard and results merge — the scatter/gather strategy.
Higher bandwidth, lower latency for multi-hop local patterns. The two phases
are interchangeable per-fragment because they compute the same bag (B2/B4).

**Consistency:** writes are already synchronous and replicated; reads follow
the owner-first + failover order, so read-your-writes holds per gateway as it
does today. Distributed `MERGE`/constraints remain non-goals for v1 (§14).

---

## 11. File-by-File Change Plan

```
src/graph.lisp               NEW  Group A axioms; entity structs; record encode/decode;
                                   key schemas; derived indexes + rebuild; invariants;
                                   memory-graph backend.            (~450 lines)
src/cypher/conditions.lisp   NEW  cypher-error hierarchy + restarts.        (~80)
src/cypher/lexer.lisp        NEW  pure tokenizer.                          (~250)
src/cypher/parser.lisp       NEW  recursive descent -> AST.                (~600)
src/cypher/ast.lisp          NEW  AST constructors/accessors/printer
                                   (print/read round-trip law).            (~150)
src/cypher/semantics.lisp    NEW  scopes, checks, constant folding.        (~400)
src/cypher/algebra.lisp      NEW  C2 operator descriptors + law docstrings. (~300)
src/cypher/planner.lisp      NEW  B4 decomposition, join order, pushdown.  (~450)
src/cypher/executor.lisp     NEW  pull cursors; bindings; aggregates.      (~600)
src/cypher/functions.lisp    NEW  scalar + aggregate function library.     (~500)
src/cypher/updates.lisp      NEW  effect algebra C3 + transaction executor. (~350)
src/cypher/reference.lisp    NEW  metacircular oracle [[.]]               (~150)
src/cypher/reader.lisp       NEW  scoped readtable pattern syntax.         (~150)
src/cypher/api.lisp          NEW  cypher entry point, defcypher macro,
                                   tcp-graph / gateway-graph backends.     (~300)
src/protocol.lisp            EDIT opcodes 12..16, layouts, docstring table
src/node.lisp                EDIT dispatch arms for 12..16
src/gateway.lisp             EDIT gateway-cypher + primitive routing
src/cluster.lisp             EDIT cluster-cypher (tests)
src/api.lisp                 EDIT cypher client function
src/web.lisp                 EDIT /api/cypher route + run-command "cypher"
src/package.lisp             EDIT exports (cypher, graph-*, conditions)
scalaxy.asd                  EDIT add modules (serial order: graph before cypher)
tests/cypher-parser.lisp     NEW round-trip + error-position tests
tests/cypher-algebra.lisp    NEW law tests (Appendix B)
tests/cypher-oracle.lisp     NEW differential tests (optimized vs reference)
tests/cypher-scenarios.lisp  NEW TCK-style scenario runner + scenarios/*.cypher
tests/cypher-distributed.lisp NEW gateway/cluster cypher tests
tests/run-tests.lisp         EDIT register new groups (keeps the no-framework harness)
web/index.html, app.js, app.css EDIT console hint + Graph tab
docs/cypher-reference.md     NEW language reference (scope, grammar, functions)
docs/rest-api.md, README.md, CHANGELOG.md  EDIT document the new surface
```

Estimated total: ~5,300 lines of new Common Lisp + ~1,500 lines of tests,
in the existing style (2-space indent, docstrings, plists for messages).

---

## 12. Testing: The Axioms Become Executable Artifacts

### 12.1 Law tests (Appendix B catalogue)

Each algebra law becomes a `check` in `tests/cypher-algebra.lisp`, run over
randomly generated graphs/tables (deterministic generator seeded with
`splitmix64` — reusing the in-tree finalizer; no external property-testing
library):

- L1 filter distributes over union; L2 project/filter commutation;
- L3 join associativity/commutativity (bag equality, multiplicity-sensitive);
- L4 `distinct` idempotence; L5 skip+limit fusion; L6 `UNWIND` of a singleton;
- L7 `OPTIONAL MATCH` = left outer join with NULL fill;
- L10 grouping-key axiom (WITH semantics); L12 expand = adjacency pullback;
- L11/L13 pattern decomposition ≡ hom-set (oracle equivalence);
- L14 `print ∘ parse = id` on canonical forms; L14' string syntax ≡ reader
  syntax (same AST); L15 effect laws: create∘delete restores the graph,
  `merge;merge = merge`, set∘remove = absent;
- L16 routing axiom: `ring-lookup(element-key)` = element's owner for every
  key the graph layer ever writes.

### 12.2 Differential oracle tests

`tests/cypher-oracle.lisp` generates random graphs (n nodes, m labeled edges,
random properties), random patterns/queries within the grammar, and checks
`reference-eval(query) ≡ executor-eval(query)` as bags of projected bindings.
The reference evaluator is the specification — a mismatch is a bug in the
optimized path, by definition.

### 12.3 TCK-style scenario tests

`tests/cypher-scenarios.lisp` reads small `scenarios/*.cypher` files
(query + expected rows) with a one-paragraph scenario format; each scenario
is a `check`. Seeds the corpus with the classic examples: homomorphism
re-binding (`(a)-->(b)-->(c)` with `a = c` allowed), NULL propagation in
WHERE, aggregation over empty input, `OPTIONAL MATCH` NULL fill, `UNWIND`
explosion, `ORDER BY` stability with SKIP/LIMIT, `MERGE` idempotence,
`DETACH DELETE` totality (A1), missing-property NULL reads.

### 12.4 Invariant tests

- T1: after any random mutation sequence, `graph-check-invariants` passes
  (A1–A3: rel endpoints exist; one rel type; props well-formed).
- T2: after `make-store` replay of a mutated log, graph state and rebuilt
  indexes equal the pre-crash state (A0-log + D2).
- T3: planner stats rebuilt by `δ` equal stats collected incrementally
  (index/stats consistency).
- T4: crash-mid-mutation simulation (truncated log tail) still replays to a
  valid graph (D1/D2).

### 12.5 Expected test volume

| Group | New checks (approx.) |
|---|---|
| parser round-trip + errors | 150 |
| algebra law tests | 200 |
| differential oracle | 250 |
| TCK-style scenarios | 300 |
| storage/invariant tests | 150 |
| distributed (gateway/cluster) | 150 |
| functions/expressions | 250 |
| **Total** | **~1,450** (suite grows 8,654 → ~10,100) |

All in the existing dependency-free harness (`deftest`/`check`), keeping
`make test` the single green/red gate.

---

## 13. Milestones and Acceptance Criteria

| Milestone | Content | Acceptance |
|---|---|---|
| M0 | This document + axiom catalogue review | Axioms and laws agreed |
| M1 | `graph.lisp`: entities, KV encoding, indexes, rebuild, invariants | T1–T4 green; records visible in data browser |
| M2 | Lexer + parser + AST + printer + conditions | Round-trip law green; error positions tested |
| M3 | Semantics + planner + executor for Tier 1 read-only core (MATCH/WHERE/RETURN/WITH/UNWIND/ORDER/SKIP/LIMIT/DISTINCT) | Differential oracle tests green on random queries |
| M4 | Functions + expressions complete (incl. 3VL, CASE, predicates) | Function/conformance checks green |
| M5 | Tier 2 effect algebra (CREATE/MERGE/SET/REMOVE/DELETE/DETACH) + `+op-cas+` | Effect law tests green; A1 enforced |
| M6 | Protocol opcodes 12..16, node dispatch, gateway-cypher, client `cypher`, `POST /api/cypher`, console command | Distributed tier-1 scenarios green over TCP cluster |
| M7 | Tier 3 (variable-length patterns, shortestPath, comprehensions) + query shipping + Graph tab | BFS/limited-expansion tests green |
| M8 | TCK corpus, `docs/cypher-reference.md`, README/changelog/docs updates, release | Full suite green; docs merged |

Each milestone is independently shippable and keeps `make test` green.

---

## 14. Risks and Mitigations

| Risk | Mitigation |
|---|---|
| CL `NIL` vs Cypher NULL confusion | Dedicated `+cypher-null+` object (never NIL); conversion discipline at JSON/wire boundaries; tests for every NULL path (§4.6) |
| Bag/multiplicity subtleties (join multiplicity, DISTINCT placement) | Reference oracle defines bags; law tests L3/L4 pin multiplicities |
| Homomorphism vs embedding confusion (re-binding `a=c`) | B2 stated explicitly; scenario tests encode it |
| Parser/scope creep vs full openCypher | Grammar-scoped tiers (§5.1); TCK-pinned subset; errors say "unsupported in Scalaxy v1" via conditions |
| Alist-binding performance | Optimized column-environment path; purity only at boundaries (§7.2) |
| Per-node counter / MERGE races | Per-node lock in v1; `+op-cas+` in Tier 2; distributed MERGE deferred (§9.1, non-goal) |
| Startup index rebuild cost | O(n) rebuild is honest v1 behavior; Phase-3 logged index records optional (§8.3) |
| Distributed join bandwidth | Filter pushdown (C4) + batching (`+op-mget+`) + Phase B query shipping |
| Write amplification from index keys | Indexes are derived, optional, rebuildable; batch lowerings per mutation |

## 15. Non-Goals (v1)

Full GQL conformance; stored procedures/APOC-style libraries; cost-based
planner with histograms (heuristics + derived stats only); multi-key
transactions (already a separate roadmap item); distributed `MERGE`/unique
constraints; graph algorithms library (shortestPath only); secondary
property indexes (Phase 4); query result caching.

---

## Appendix A — Full Key Schema

| Key | Value | Written by | Read by |
|---|---|---|---|
| `n:<eid>` | node record (JSON octets) | CREATE/MERGE/SET/REMOVE | point gets, scans |
| `r:<eid>` | rel record (JSON octets) | CREATE/MERGE/SET/REMOVE | point gets, bulk `+op-mget+` |
| `nl:<label>:<eid>` | `""` | CREATE/SET labels | label scan (prefix `nl:<label>:`) |
| `e:<eid>:o:<type>:<rid>` | `""` | CREATE rel | expand out (`e:<eid>:o` prefix) |
| `e:<eid>:i:<type>:<rid>` | `""` | CREATE rel | expand in, DETACH DELETE |
| `g:next:<node-id>` | u64 text | oid minting | minting |
| `ep:<label>:<prop>:<enc-val>:<eid>` | `""` (Phase 4) | property index ops | property-lookup scan |

## Appendix B — Algebra Law Catalogue (test hooks)

```
L1  filter(p, T1 ∪ T2)          = filter(p,T1) ∪ filter(p,T2)
L2  project(V, filter(p,T))     = filter(p, project(V,T))    (p uses only V)
L3  join(T1,T2,k)  associative and commutative (bags, multiplicity exact)
L4  distinct ∘ distinct = distinct
L5  limit(l, skip(k, T)) = slice(k,l,T)
L6  unwind([x], v) = rename/keep (singleton identity)
L7  optional(T1,T2,k) = left-outer-join with NULL fill on unmatched
L8  WITH projection is transparent when no aggregation
L9  sort is a total stable order
L10 grouping keys = non-aggregate vars; empty input -> count=0, others NULL
L11 [[PATTERN]] = ⋈κ over fragment match bags            (B4)
L12 expand(T,v,r,v',dir,types) = T ⋈ {(r,w) | src/dst per dir, type match}
    — the adjacency pullback
L13 reference-eval(q) ≡ executor-eval(q) for all q        (oracle)
L14 print ∘ parse = id (canonical AST); L14' string AST ≡ reader AST
L15 create∘delete restores graph; merge;merge = merge; set∘remove = absent
L16 ring-lookup(key) = owner for every key the graph layer writes
```

## Appendix C — References

- openCypher specification (CIP2015-11-26) and TCK scenario corpus
- ISO/IEC 39075 (GQL, 2024) — the standardized Cypher-family language; the
  subset here is chosen to be a GQL-aligned core
- The "property graph algebra" line of work (relational-algebra treatment of
  property graphs; Codd-style operator/law presentation adopted in §4.3)
- P. Norvig, *Paradigms of AI Programming* — the "interpreter as
  specification" method (metacircular evaluator, §7.4)
- G. Kiczales et al., *The Art of the Metaobject Protocol* — open-set
  dispatch as design principle (§7.3)
- Common Lisp standard (ANSI X3.226-1994): reader/readtable (§6.3),
  conditions/restarts (§7.6), packages, CLOS
- Scalaxy in-tree: `docs/protocol.md`, `docs/rest-api.md`, `CONTRIBUTING.md`,
  `src/protocol.lisp` (A0-wire), `src/storage.lisp` (A0-log),
  `src/consistent-hash.lisp` (A0-ring)

---

*End of plan. Companion documents to be produced per milestone:
`docs/cypher-reference.md` (M8), `src/cypher/*.lisp` docstrings (the axioms
A1–A3, B1–B4, C1–C4, D1–D4, E1–E4 live with their implementations).*
