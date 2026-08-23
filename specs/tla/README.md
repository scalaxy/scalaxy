# Scalaxy TLA+ Certification

`ScalaxySpec.tla` is the **central truth** for how Scalaxy must be implemented.
Any implementation behavior that contradicts this specification is a bug.

## Verification status

Machine-verified with TLC (TLA+ model checker), exhaustive breadth-first search:

| Model | States explored | Result |
|-------|----------------|--------|
| 2 nodes x 2 keys | 36 | PASS |
| 3 nodes x 3 keys | 3,424 | PASS |

All seven invariants hold over every reachable state:

| Invariant | Scope | Guarantee |
|-----------|-------|-----------|
| `TypeOK` | correctness | All state well-formed |
| `DataIntegrity` | storage | Every live key has >=1 durable copy, regardless of crashes |
| `DeleteVisible` | correctness | Deleted keys can never resurrect |
| `AvailabilityUnderSingleFailure` | reliability | Any single-node crash costs zero read availability |
| `RecoveryRestoresService` | reliability | Crashed nodes recover with all durable copies intact |
| `ReplicationFactorTwo` | storage | Live keys always stored on >=2 nodes (RF=2) |
| `EncryptionAtRest` | security | No key ever stored on an unencrypted node |

## Central-truth rules binding the implementation

1. **COMMIT RULE** -- A write commits only after the key is durably placed
   on two DISTINCT nodes that are up AND have encryption enabled.
2. **DELETE RULE** -- Delete sets the tombstone immediately; the key becomes
   invisible atomically and can never be written again.
3. **SECURITY RULE** -- Encryption at rest may be disabled on a node only
   after every durable copy has been drained from it. Plaintext data must
   never exist on any disk.
4. **RECOVERY RULE** -- Node restart restores its durable copies intact;
   recovery never requires network access to other nodes.

TLC found three bugs in earlier drafts of this specification itself,
demonstrating the central-truth process:

1. A write action without explicit data placement violated `DataIntegrity`
   -> writes MUST place data before commit.
2. A write action missing the tombstone guard allowed deleted keys to
   resurrect -> tombstones are permanent.
3. An availability invariant claiming "any surviving node implies readable"
   was falsified under RF=2 with 3 nodes -> the honest guarantee is
   single-failure tolerance, now stated exactly.

## How to verify

Requires Docker (no local Java needed):

    docker run --rm -v "$(pwd)/specs/tla:/specs" amazoncorretto:17 \
      sh -c "cd /specs && java -cp tla2tools.jar tlc2.TLC \
             -config ScalaxySpec.cfg ScalaxySpec.tla"

Expected output: `Model checking completed. No error has been found.`

Parse-check only (fast):

    docker run --rm -v "$(pwd)/specs/tla:/specs" amazoncorretto:17 \
      sh -c "cd /specs && java -cp tla2tools.jar tla2sany.SANY ScalaxySpec.tla"

`tla2tools.jar` (v1.8.0) must be present in `specs/tla/`; it is gitignored --
download from <https://github.com/tlaplus/tlaplus/releases>.

## Legacy modules

`ScalaxyStorage.tla`, `ScalaxyReplication.tla`, `ScalaxyEncryption.tla`, and
`ScalaxyAggregate.tla` are earlier per-concern modules kept for reference.
`ScalaxySpec.tla` supersedes them as the single source of truth.
