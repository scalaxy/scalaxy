# Critical analysis of ScalaxySpec.tla (v1) and how v2 fixes it

Review date: 2026-08-23. Reviewer: TLA+ certification pass 2.

## Issues found in v1

### 1. CRITICAL -- Atomic two-node writes are unbuildable
`MakeLive(k, w1, w2)` placed both replicas in ONE atomic step. No real
distributed system can write two machines atomically. As "central truth"
the spec mandated something unimplementable, and it structurally disagreed
with the implementation, which does: local durable write -> durable outbox
entry -> background shipper copies to peer.

**v2 fix**: writes place ONE copy and enqueue an outbox entry; a separate
`ShipReplica` action performs replication asynchronously.

### 2. CRITICAL -- The single-copy risk window was inexpressible
Because v1 wrote both copies atomically, the model could not represent the
window where a committed key has only one durable copy -- exactly the
window that exists in every real replication protocol and where data is
most at risk. Spec and implementation disagreed about whether this window
exists at all.

**v2 fix**: the window exists in the model; safety invariants are checked
across it. `DataIntegrity` now means ">= 1 durable copy at all times"
(not >= 2), which is what asynchronous replication can actually promise.

### 3. HIGH -- No liveness: nothing forced replication to finish
v1 checked only safety. A scheduler that starved the shipper forever would
pass every invariant while data stayed under-replicated forever.

**v2 fix**: weak fairness on `ShipReplica` and `RecoverNode`, plus the
temporal property `ReplicationConverges`: under fair scheduling every live
key eventually reaches RF durable copies.

### 4. MEDIUM -- Delete visibility was globally instantaneous
One shared `live[k]` flag makes deletion visible everywhere atomically.
Real propagation takes time; stale reads are possible in the window. v2
keeps the global-flag abstraction but documents it explicitly as an
abstraction of visibility propagation rather than an implementable claim.

### 5. LOW -- Encryption could be disabled but never re-enabled
**v2 fix**: `EnableEncryption(n)` allowed on a drained node, symmetric with
disable. `EncryptionAtRest` still verified over all reachable states.

### 6. HYGIENE -- Superseded legacy modules left unverified
ScalaxyStorage/Replication/Encryption/Aggregate .tla files predate the
unified spec, were never TLC-verified, and invite confusion about which
file is truth. Moved to `specs/tla/legacy/` and marked non-normative.

## What v2 still abstracts away (documented limitations)

- Disk/media loss: crashes preserve `held`. Justified by S3 durability +
  local cache self-healing, but a corrupt-disk action would falsify plain
  DataIntegrity; the honest claim is "durable given at least one intact
  replica", which RF=2 + repair provides operationally.
- Value contents and read staleness: single logical value per key;
  per-node read lag is folded into the delete-visibility abstraction.
- Owner selection: real Scalaxy picks peers via consistent hashing; the
  model lets the shipper choose any node without a copy (existential).
  This weakens nothing the invariants claim.

## Findings TLC caught WHILE building v2 (2026-08-23)

The v2 review pass itself produced counterexamples -- evidence the process
keeps working even on the reviewer's own drafts:

1. **Overstated availability (again).** v1's claim "any single-node crash
   costs zero availability" was carried into v2 unchanged and was
   falsified immediately: a key written to its sole holder, followed by
   that node crashing, leaves no running holder during the replication
   window. Replaced with the honest statement:
   `AvailabilityUnderSingleFailureAfterReplication` -- after convergence,
   single-failure reads stay available; before convergence a crash costs
   temporary availability until recovery.

2. **A broken "recovery" safety invariant.** The first attempt to state
   "recovery restores service" as a safety invariant was logically wrong
   (it required every downed node to hold every live key) and was caught
   by a total-outage-with-empty-node-down counterexample. Reformulated
   correctly as the temporal property `ServiceRestored`:
   every live key eventually becomes readable again under fair recovery.

## Verification matrix (v2, all PASS)

| Config | Distinct states | Liveness | Time |
|--------|----------------|----------|------|
| 2 nodes x 1 key x RF=2 | 56 + temporal | yes | <1 s |
| 2 nodes x 2 keys x RF=2 | 272 + temporal | yes | 1 s |
| 3 nodes x 2 keys x RF=2 | 2,752 + temporal | yes | 2 s |
| 3 nodes x 3 keys x RF=2 | 26,464 + temporal | yes | 9 s |
| 3 nodes x 3 keys x RF=3 | 92,240 + temporal | yes | 33 s |

---

# Pass 3 (2026-08-23): media loss + anti-entropy self-healing

## Remaining realism gaps found in v2

1. **HIGH -- disks never died.** `held` survived everything, so the spec
   claimed unconditional durability. Real clusters lose disks; Scalaxy's
   own self-healing cache/replica validation exists precisely because of
   this. A central-truth spec that cannot express data loss cannot state
   what RF=2 actually buys.

2. **HIGH -- no anti-entropy.** Replication depended solely on the write
   path's outbox entry. Lose that entry (e.g., with its disk) and a key
   stays under-replicated forever. Real systems repair from any holder.

## What was added

- `LoseDisk(n)`: permanent media-loss action; wipes held+outbox, takes
  the node down.
- `lostData[k]`: latching history flag -- TRUE forever once the last copy
  of a live key is destroyed. Honesty mechanism, not an excuse: invariants
  now say exactly when data is gone.
- `ReReplicate(k, src, dst)`: anti-entropy repair from any holder to any
  up, encrypted node lacking a copy whenever holders < Rf. Fairness-
  scheduled like the shipper.
- Invariants restated honestly:
  - `DataIntegrityUnlessMediaLoss` (was unconditional DataIntegrity)
  - `NoFalseLossAlarm` / `NoUndetectedLoss` (two-directional accounting)
- Liveness guarded by media loss: convergence and service restoration
  hold for every key EXCEPT those whose last copy was destroyed --
  which is precisely the promise any storage system can truthfully make.

## Bugs TLC caught in THIS pass's drafts

1. **Latch reset bug**: the first `LoseDisk` recomputed lostData from
   scratch instead of OR-ing with the old value, so a second disk loss
   flipped lostData[k] back to FALSE after it had correctly latched TRUE.
   TLC counterexample: write -> lose disk 1 (lostData=TRUE) -> lose disk 2
   (lostData wrongly reverted). Fixed by proper latching.

2. **Biconditional overreach**: stating "lostData[k] <=> live and zero
   copies" broke when a lost key was later deleted (live=FALSE while
   lostData stays latched). Replaced with two separate implications,
   each verified.

## Verification matrix (pass 3, all PASS, safety + temporal)

| Config | Distinct states | Time |
|--------|----------------|------|
| 2 nodes x 1 key x RF=2 | 152 | <1 s |
| 2 nodes x 2 keys x RF=2 | 1,840 | 1 s |
| 3 nodes x 2 keys x RF=2 | 19,808 | 8 s |
| 3 nodes x 2 keys x RF=3 | 23,552 | 9 s |
| 3 nodes x 3 keys x RF=2 | 525,632 | 395 s |

---

# Pass 4 (2026-08-24): topology, zone failures, and the five-nines claim

## New requirement

Prove that under node/zone outages the cluster can carry a healthy load
without interruption -- telecom-grade ("five nines") -- GIVEN proper
topology. This forced topology into the model: v3 was topology-blind
(replicas could share a rack/zone), so no zone-level claim was even
stateable.

## What was added

- `ZoneOneNodes` / `ZoneTwoNodes` constants partitioning `Nodes`
  (ASSUME-checked partition; two zones suffice for RF=2 claims).
- `CrossZone(src, dst)` guard on BOTH `ShipReplica` and `ReReplicate` --
  proper topology is now a central-truth RULE: replicas never share a
  zone, ever, on any path.
- `ZoneOutage(Z)` / `ZoneRecover(Z)`: whole-zone failure and recovery.
- Invariants: `ZoneSpreadReplication` (birthright form, suspended by the
  `degraded` history flag after media loss), 
  `AvailabilityUnderSingleZoneFailure`.
- Liveness: `TopologyHeals` -- anti-entropy re-spreads surviving keys
  after media loss; `ReReplicate` trigger strengthened to fire whenever
  coverage is lost, not merely count.

## Bugs TLC caught in THIS pass's drafts

1. **Overbroad zone-failure claim**: "at most one zone fully down =>
   all readable" was falsified immediately -- after a zone outage,
   PARTIAL node crashes inside the SURVIVING zone can still take out
   exactly the nodes holding a key's copies. Corrected by requiring the
   surviving zone to be healthy (`SomeZoneFullyUp`); partial single-node
   crashes inside it remain covered by the existing single-node-failure
   invariant. The honest combined statement: converged service survives
   ANY one full-zone outage OR any one node crash.
2. **Post-loss placement drift**: after media loss, the remaining Rf
   holders can all sit in ONE zone (e.g., 3 holders spread over 2 zones,
   lose the lone zone-B holder -> 2 holders both in zone A). A state-
   level "always zone-spread" invariant is simply FALSE in those states;
   TLC found the counterexample. Reformulated honestly: zone-spread is a
   birthright until first media loss (~degraded antecedent) and a
   LIVENESS obligation afterwards (TopologyHeals).

## Verification matrix (pass 4, all PASS, safety + temporal)

| Config | Distinct states | Time |
|--------|----------------|------|
| 2 nodes (1+1 zones) x 1 key x RF=2 | 224 | 1 s |
| 3 nodes (2+1 zones) x 2 keys x RF=2 | 22,432 | 17 s |
| 3 nodes (2+1 zones) x 3 keys x RF=2 | 31,688 | 16 s |
| 4 nodes (2+2 zones) x 2 keys x RF=2 | 211,200 | 350 s |

## The five-nines argument (structural proof + arithmetic)

Five nines = 99.999% availability = at most 5.26 minutes of downtime per
year (8,760 h budget x 10^-5).

STRUCTURAL HALF (machine-proven): `AvailabilityUnderSingleZoneFailure`
-- with proper (cross-zone) topology, a full outage of any ONE zone,
with the surviving zone healthy, leaves EVERY live key readable. No
configuration change, no operator action: the surviving zone already
holds a copy of every key because placement never shares a zone.

ARITHMETIC HALF (standard fault-tree math, operational inputs):
service-affecting downtime requires TWO independent zone failures
overlapping in time. Expected overlap per year ~= (d1 x d2) / 8,760
hours, where di = annualized zone downtime:

  - Conservative zones, d = 24 h/yr each:
      24 x 24 / 8,760 = 0.0658 h/yr = 3.9 min/yr  < 5.26 min  => five nines MET
  - Typical cloud-zone availability (99.99%, d ~= 8.8 h/yr):
      8.8 x 8.8 / 8,760 = 0.0088 h/yr = 32 s/yr   => ~99.9999%

ASSUMPTIONS (stated, not hidden): zone failures statistically
independent (no shared power/network/control plane across zones);
bounded repair time; the replication window (before a key converges to
cross-zone copies) is covered by the separately-proven recovery
guarantees; clients implement failover.

This is exactly how telecom carriers certify five nines: prove that no
single fault is service-affecting (we did, mechanically), then bound the
probability of simultaneous faults arithmetically.

---

# Pass 5 (2026-08-24): inter-zone partitions and the write side of the workload

## Remaining gaps found in pass 4

1. **HIGH -- the inter-zone network could not fail.** Whole-zone outages were
   modeled, but the far more common telecom failure -- a partition where both
   zones stay UP but cannot talk to each other -- was inexpressible. Real
   always-on systems must keep serving through exactly this.
2. **MEDIUM -- "healthy workload" only covered reads.** Nothing stated that
   clients can still WRITE during degraded operation.

## What was added

- `linkUp` variable + `LinkPartition` / `LinkHeal` actions: every node keeps
  running; cross-zone shipping and repair pause until the link heals.
- Both cross-zone actions gated on `linkUp`.
- `WriteCapabilityUnderSingleZoneFailure`: under a single-zone outage with a
  healthy survivor, some surviving node accepts (or can instantly accept)
  writes -- reads AND writes carry on. Liveness properties now honestly
  guarded on link health (`[](... /\ linkUp) => ...`): if the link never
  heals, convergence is not promised -- which is the truth.

## Bug TLC caught in THIS pass's draft

The first write-capability invariant demanded an already-ENCRYPTED survivor
and was falsified immediately: an operator may disable encryption on a
DRAINED node whose zone then dies, leaving an up-but-unencrypted survivor.
Per the security rule, re-enabling on a drained node is instant and safe, so
the honest capability condition is "encrypted OR drained". A node holding
data is always encrypted, so every healthy up node qualifies.

## Verification matrix (pass 5, all PASS)

| Config | Distinct states | Time |
|--------|----------------|------|
| 3 nodes (2+1 zones) x 2 keys x RF=2, WITH partitions | 44,864 | 37 s |
| 4 nodes (2+2 zones) x 2 keys x RF=2, WITH partitions | 422,400 | 781 s |

Partition modeling roughly doubled the state space versus pass 4 (same
configs were 22K / 211K), and every invariant held in every partitioned
state: a split cluster loses nothing -- it merely pauses convergence.

## Updated five-nines story

The structural claim now covers the full failure taxonomy:
node crash, whole-zone outage, disk loss (with self-healing), AND
inter-zone partition. Reads continue in all cases from local copies;
writes continue wherever an up encrypted-or-drainable node exists;
convergence resumes when the disturbance ends (proven under fair
scheduling with link health). The arithmetic half is unchanged --
service-affecting events still require two independent zone-level
failures overlapping within the repair window.
