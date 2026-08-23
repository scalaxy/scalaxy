# Scalaxy TLA+ Certification

`ScalsyxSpec.tla` -> `specs/tla/ScalaxySpec.tla` is the **central truth** for
how Scalaxy must be implemented. Any implementation behavior that contradicts
this specification is a bug.

> NOTE: file is `ScalaxySpec.tla`; header comment kept short on purpose.

## What v2 models (and why it changed)

v1 mandated atomic two-node writes -- unbuildable in any real system. v2
models the actual architecture: local durable write + durable outbox +
asynchronous background shipper. See `CRITIQUE.md` for the full critical
analysis, including two additional overstatements TLC caught during the
v2 review pass itself.

Central-truth rules binding the implementation:

1. **WRITE RULE** -- a write becomes visible once its FIRST durable
   encrypted copy exists; reaching RF copies is asynchronous via the
   durable outbox.
2. **REPLICATION RULE** -- the shipper copies under-replicated keys only
   to up, encrypted nodes; the outbox entry retires at RF copies.
3. **DELETE RULE** -- tombstone immediate and permanent; deleted keys can
   never be written again.
4. **SECURITY RULE** -- encryption toggles allowed only while a node holds
   no data. Plaintext never exists on disk.
5. **FAIRNESS RULE** -- shipper and recovery are fairly scheduled:
   under-replicated data converges to RF copies.

## Verification status

All runs: TLC exhaustive breadth-first search, safety + liveness
(temporal) checking. Every configuration below PASSED with zero errors.

| Config | Distinct states | Liveness | Time |
|--------|----------------|----------|------|
| 2 nodes x 1 key x RF=2 | 56 | checked | <1 s |
| 2 nodes x 2 keys x RF=2 | 272 | checked | 1 s |
| 3 nodes x 2 keys x RF=2 | 2,752 | checked | 2 s |
| 3 nodes x 3 keys x RF=2 | 26,464 | checked | 9 s |
| 3 nodes x 3 keys x RF=3 | 92,240 | checked | 33 s |

Safety invariants (hold in every reachable state of every config):

| Invariant | Scope | Guarantee |
|-----------|-------|-----------|
| `TypeOK` | correctness | All state well-formed |
| `DataIntegrity` | storage | Visible keys always have >= 1 durable copy, even mid-replication |
| `DeleteVisible` | correctness | Deleted keys can never resurrect |
| `EncryptionAtRest` | security | No key ever stored on an unencrypted node |
| `AvailabilityUnderSingleFailureAfterReplication` | reliability | After convergence, single-node crash costs zero availability |
| `RecoveryRestoresService` | storage | Crashes never destroy durable copies |

Liveness properties (checked under WF fairness):

| Property | Guarantee |
|----------|-----------|
| `ReplicationConverges` | Stable cluster => every live key reaches RF copies |
| `ServiceRestored` | Every live key eventually readable again after crashes |

## Honest limitations (see CRITIQUE.md)

- Before replication converges, crashing the sole holder costs TEMPORARY
  unavailability until recovery (data survives). TLC proved this is real;
  the spec states it instead of hiding it.
- Per-node delete-visibility propagation delay is abstracted to one flag.
- Disk/media corruption is out of scope; durability claim is conditional
  on at least one intact replica surviving (operationally backed by RF=2).

## How to verify

    docker run --rm -v "$(pwd)/specs/tla:/specs" amazoncorretto:17 \
      sh -c "cd /specs && java -cp tla2tools.jar tlc2.TLC \
             -config ScalaxySpec.cfg ScalaxySpec.tla"

Expected: `Model checking completed. No error has been found.`

Parse-only: replace `tlc2.TLC -config ScalaxySpec.cfg` with
`tla2sany.SANY`. Requires `tla2tools.jar` v1.8.0 in `specs/tla/`
(gitignored; from github.com/tlaplus/tlaplus/releases).

## Legacy modules

`legacy/` contains pre-unification per-concern modules. They are NOT
normative and were never fully verified; `ScalaxySpec.tla` supersedes them.
