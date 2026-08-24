# Scalaxy TLA+ Certification

`specs/tla/ScalaxySpec.tla` is the **central truth** for how Scalaxy must
be implemented. Any implementation behavior that contradicts
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

| Config | Topology (zones) | Distinct states | Time |
|--------|------------------|-----------------|------|
| 3 nodes x 2 keys x RF=2 | z1:{n1,n2}, z2:{n3} | 22,432 | 17 s |
| 3 nodes x 3 keys x RF=2 | z1:{n1,n2}, z2:{n3} | 31,688 | 16 s |
| 4 nodes x 2 keys x RF=2 | z1:{n1,n2}, z2:{n3,n4} | 211,200 | 350 s |

All runs check safety invariants AND temporal properties
(ReplicationConverges, ServiceRestored, TopologyHeals).

Model includes media loss (`LoseDisk`), anti-entropy self-healing
(`ReReplicate`), availability zones and whole-zone outages
(`ZoneOutage`). See CRITIQUE.md passes 3-4.

Safety invariants (hold in every reachable state of every config):

| Invariant | Scope | Guarantee |
|-----------|-------|-----------|
| `TypeOK` | correctness | All state well-formed |
| `DataIntegrityUnlessMediaLoss` | storage | Visible keys keep >= 1 durable copy unless media loss destroyed the last one (then flagged) |
| `NoFalseLossAlarm` / `NoUndetectedLoss` | storage | Media loss accounted exactly, both directions |
| `ZoneSpreadReplication` | topology | Until first media loss, replicated copies never share a zone |
| `AvailabilityUnderSingleZoneFailure` | reliability | Full outage of any ONE zone (survivor healthy) never interrupts converged service |
| `DeleteVisible` | correctness | Deleted keys can never resurrect |
| `EncryptionAtRest` | security | No key ever stored on an unencrypted node |
| `AvailabilityUnderSingleFailureAfterReplication` | reliability | After convergence, single-node crash costs zero availability |

Liveness properties (checked under WF fairness):

| Property | Guarantee |
|----------|-----------|
| `ReplicationConverges` | Stable cluster => every live key reaches RF copies (unless media-lost) |
| `ServiceRestored` | Every live key eventually readable again after crashes (unless media-lost) |
| `TopologyHeals` | Stable cluster re-spreads surviving keys across zones after media loss |

## Honest limitations (see CRITIQUE.md)

- Before replication converges, crashing the sole holder costs TEMPORARY
  unavailability until recovery (data survives). TLC proved this is real;
  the spec states it instead of hiding it.
- Per-node delete-visibility propagation delay is abstracted to one flag.
- Media loss IS modeled (pass 3): losing the last copy of a live key is
  permanent and flagged via lostData; all other losses self-heal via
  anti-entropy replication.

## Five nines (telecom-grade availability)

Five nines = at most 5.26 minutes downtime per year. The certification
argument has two halves:

**Structural half -- machine-proven.** With proper topology (replicas
never share a zone -- enforced on EVERY replication path), a full outage
of any one zone with the surviving zone healthy leaves every live key
readable from the survivor. TLC verified this invariant over all
reachable states of the certified models. No single-zone event is
service-affecting.

**Arithmetic half -- standard fault-tree math.** Downtime therefore
requires two independent zone failures overlapping within the repair
window: expected overlap ~= d1*d2 / 8760 hours per year.

    Conservative zones (24 h/yr each):   3.9 min/yr  < 5.26  => five nines MET
    Typical cloud zones (8.8 h/yr each): 32 sec/yr          => ~99.9999%

Assumptions: independent zone failures (no shared power/network/control
plane), bounded repair, converged keys (the replication window is
covered by the proven recovery guarantees), client failover.

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
