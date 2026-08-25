# Known issues found by scripts/tla-conformance.py (live-cluster probes)

## KI-1: fast-counts are stale on freshly restarted lazy-S3 nodes

`MATCH (n) RETURN count(n)` returns 0 (the boot-time value of an empty
index) until background summaries recompute, even though writes landed.
Exact-key property reads (`MATCH (n {id:"..."})`) see the data
immediately. Root cause: `make-local-graph` skips
`graph-rebuild-indexes` whenever a store backend exists, and the count
fast path reads the (empty) in-memory index. Writes invalidate
`s3-summary-valid` without scheduling a recompute.

## KI-2: records written via HTTP fail to decode after cache eviction

Repro: POST /api/cypher `CREATE (:ConfTest {id:"k"})` -> immediate
property read OK (in-memory hash table). Later -- after node restart or
cache pressure -- reading the same key raises
`graph: corrupt record (not a map): #(8 0 0 0 2 ...)` on every node,
including nodes holding replicated copies. The persisted byte encoding
of newly written records does not match what `%decode-record` expects on
reload. Pre-existing data imported offline reads fine; only data written
through the HTTP update path is affected.

Impact: TLA+ invariant `DataIntegrityUnlessMediaLoss` holds in the model
but is violated by the implementation for HTTP-written keys across
restarts. Conformance checks `RecoveryRestoresService`,
`ReplicationConverges`, `AvailabilityUnderSingleFailureAfterReplication`
and `AvailabilityUnderSingleZoneFailure` fail as a consequence once
probes cross a reload boundary.

Both issues predate this harness. They are implementation bugs relative
to the central-truth specification -- exactly the class of divergence
the certification process exists to catch.
