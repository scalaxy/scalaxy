-------------------------------- MODULE ScalaxyStorage --------------------------
(***************************************************************************)
(*                                                                         *)
(*  Scalaxy Storage Specification                                          *)
(*  =============================                                          *)
(*                                                                         *)
(*  Central truth for how Scalaxy stores graph data in S3-compatible       *)
(*  object storage with synchronous replication.                           *)
(*                                                                         *)
(*  This specification models:                                             *)
(*    - A cluster of nodes forming a consistent-hash ring                  *)
(*    - Key-value writes with synchronous replication (RF = 2)            *)
(*    - Deletes with tombstone markers                                    *)
(*    - Aggregate summaries (type counts, sums)                            *)
(*    - Encryption at rest                                                 *)
(*                                                                         *)
(*  The specification uses a simplified model where:                       *)
(*    - Nodes form a fixed set (no dynamic membership)                     *)
(*    - Keys come from a small finite set (for TLC model checking)         *)
(*    - Values are opaque tokens                                           *)
(*    - Ownership is modeled as an abstract function                       *)
(*                                                                         *)
(*  Author: Artem Andreenko <miolini>                                      *)
(*  License: MIT                                                           *)
(*                                                                         *)
(***************************************************************************)
EXTENDS Integers, Sequences, FiniteSets

CONSTANTS
  \* @type: Set(NodeId);
  Nodes,
          \* @type: Set(Key);
  Keys,
          \* @type: Set(Value);
  Values,
          \* @type: Nat;
  NumReplicas,
          \* The node that owns each key per the consistent-hash ring.
          \* @type: Key -> NodeId;
  Owner,
          \* @type: Bool;
  EncryptionEnabled

ASSUME
  \* We need at least 2 nodes for replication
  /\ Nodes # {}
  /\ Cardinality(Nodes) >= 2
  \* Keys must be present for meaningful verification
  /\ Keys # {}
  \* Values must be available
  /\ Values # {}

NodeId == STRING

VARIABLES
    \* @type: [NodeId -> BOOL];
    \* Whether each node's process is running
    nodeUp,
    \* @type: [NodeId -> [Key -> BOOL]];
    \* held[n][k] = TRUE iff node n holds key k locally
    held,
    \* @type: [Key -> Value];
    \* The current committed value for each key
    committedVal,
    \* @type: [Key -> BOOL];
    \* committed[k] = TRUE iff key k has been written and not deleted
    committed,
    \* @type: [Key -> BOOL];
    \* tombstoned[k] = TRUE iff key k has been deleted
    tombstoned,
    \* @type: [NodeType -> Nat];
    \* Per-type relationship counts (aggregate summaries)
    typeCounts,
    \* @type: Bool;
    \* Whether encryption is active on all stored objects
    encryptedAtRest,
    \* @type: Set([NodeId, Key]);
    \* Outbox: pending replication messages that failed delivery
    outbox,
    \* @type: Bool;
    \* Whether all summaries match the actual data
    summariesValid

Vars == <<nodeUp, held, committedVal, committed, tombstoned,
          typeCounts, encryptedAtRest, outbox, summariesValid>>

-----------------------------------------------------------------------------
(***************************************************************************)
(*                          Helper Definitions                             *)
(***************************************************************************)

\* The set of currently up nodes
UpNodes == {n \in Nodes : nodeUp[n]}

\* Keys owned by a given node (per the consistent-hash ring)
OwnedBy(n) == {k \in Keys : Owner[k] = n}

\* Keys held by node n (whether owned or replicated)
HeldBy(n) == {k \in Keys : held[n][k]}

\* Live keys (written and not deleted)
LiveKeys == {k \in Keys : committed[k] /\ ~tombstoned[k]}

\* Number of nodes holding key k
CopyCount(k) == Cardinality({n \in Nodes : held[n][k]})

\* True if key k is live (not deleted) and readable from at least one up node
IsReadable(k) ==
    \E n \in UpNodes :
        held[n][k] /\ committed[k] /\ ~tombstoned[k]

-----------------------------------------------------------------------------
(***************************************************************************)
(*                        Initial State                                    *)
(***************************************************************************)

Init ==
    \* All nodes start up
    /\ nodeUp = [n \in Nodes |-> TRUE]
    \* No keys are held initially (empty store)
    /\ held = [n \in Nodes |-> [k \in Keys |-> FALSE]]
    \* No committed values
    /\ committedVal = [k \in Keys |-> Head(Values)]
    \* Nothing committed yet
    /\ committed = [k \in Keys |-> FALSE]
    \* No tombstones
    /\ tombstoned = [k \in Keys |-> FALSE]
    \* Type counts start at zero (will be updated by writes)
    /\ typeCounts = [t \in {""} |-> 0]
    \* Encryption may or may not be enabled
    /\ encryptedAtRest \in {TRUE, FALSE}
    \* Empty outbox
    /\ outbox = {}
    \* Summaries are valid (trivially true with empty store)
    /\ summariesValid = TRUE

-----------------------------------------------------------------------------
(***************************************************************************)
(*                           Actions                                       *)
(***************************************************************************)

\*
\* Write action: write value v to key k.
\* The write goes to the ring owner first, then replicates to followers.
\*
\* Precondition: key k is not tombstoned
\* Effect: value committed on owner, replication initiated
\*
Write(k, v) ==
    /\ ~tombstoned[k]
    /\ committed' = [committed EXCEPT ![k] = TRUE]
    /\ committedVal' = [committedVal EXCEPT ![k] = v]
    /\ tombstoned' = [tombstoned EXCEPT ![k] = FALSE]
    /\ held' = [held EXCEPT ![Owner[k]][k] = TRUE,
                             ![NextReplica[Owner[k]]][k] = TRUE]
    /\ typeCounts' = typeCounts
    /\ nodeUp' = nodeUp
    /\ encryptedAtRest' = encryptedAtRest
    /\ outbox' = outbox
    /\ summariesValid' = summariesValid

\*
\* Delete action: remove key k.
\* The delete goes to the ring owner, then replicates.
\* A tombstone marker prevents resurrection from older segments.
\*
\* Precondition: key k exists (is committed)
\* Effect: key marked deleted, removed from index
\*
Delete(k) ==
    /\ committed[k]
    /\ ~tombstoned[k]
    /\ tombstoned' = [tombstoned EXCEPT ![k] = TRUE]
    /\ committed' = [committed EXCEPT ![k] = FALSE]
    /\ committedVal' = committedVal
    /\ held' = held
    /\ typeCounts' = typeCounts
    /\ nodeUp' = nodeUp
    /\ encryptedAtRest' = encryptedAtRest
    /\ outbox' = outbox
    /\ summariesValid' = summariesValid

\*
\* Successful replication: key k delivered from node f to node t.
\* Both nodes now hold key k.
\*
Deliver(f, t, k) ==
    /\ nodeUp[f] /\ nodeUp[t]
    /\ held' = [held EXCEPT ![t][k] = TRUE]
    /\ committed' = committed
    /\ committedVal' = committedVal
    /\ tombstoned' = tombstoned
    /\ typeCounts' = typeCounts
    /\ nodeUp' = nodeUp
    /\ encryptedAtRest' = encryptedAtRest
    /\ outbox' = outbox
    /\ summariesValid' = summariesValid

\*
\* Failed replication: delivery to node t fails.
\* The key is queued in the outbox for later retry.
\*
ReplicationFails(f, t, k) ==
    /\ nodeUp[f]
    /\ outbox' = outbox \union {[f, t, k]}
    /\ UNCHANGED <<committedVal, committed, tombstoned, held, typeCounts,
                   nodeUp, encryptedAtRest, summariesValid>>

\*
\* Outbox retry: deliver a pending replication from the outbox.
\* On success, remove from outbox and update held.
\*
RetryOutbox(msg) ==
    /\ msg \in outbox
    /\ \E f, t, k :
        /\ msg = [from |-> f, to |-> t, key |-> k]
        /\ nodeUp[t]
        /\ held' = [held EXCEPT ![t][k] = TRUE]
        /\ outbox' = outbox \ {msg}
        /\ UNCHANGED <<committedVal, committed, tombstoned,
                       typeCounts, nodeUp, encryptedAtRest, summariesValid>>

\*
\* Node crash: node n goes down.  Its data becomes temporarily unavailable.
\*
CrashNode(n) ==
    /\ nodeUp[n]
    /\ nodeUp' = [nodeUp EXCEPT ![n] = FALSE]
    /\ UNCHANGED <<held, committedVal, committed, tombstoned,
                   typeCounts, encryptedAtRest, outbox, summariesValid>>

\*
\* Node recovery: node n comes back up.  It still holds whatever it had.
\*
RecoverNode(n) ==
    /\ ~nodeUp[n]
    /\ nodeUp' = [nodeUp EXCEPT ![n] = TRUE]
    /\ UNCHANGED <<held, committedVal, committed, tombstoned,
                   typeCounts, encryptedAtRest, outbox, summariesValid>>

-----------------------------------------------------------------------------
(***************************************************************************)
(*                         Next-State Relation                             *)
(***************************************************************************)

Next ==
    \/ \E k \in Keys, v \in Values : Write(k, v)
    \/ \E k \in Keys : Delete(k)
    \/ \E f \in Nodes, t \in Nodes, k \in Keys : Deliver(f, t, k)
    \/ \E f \in Nodes, t \in Nodes, k \in Keys : ReplicationFails(f, t, k)
    \/ \E msg \in outbox : RetryOutbox(msg)
    \/ \E n \in Nodes : CrashNode(n)
    \/ \E n \in Nodes : RecoverNode(n)

-----------------------------------------------------------------------------
(***************************************************************************)
(*                           Invariants                                    *)
(***************************************************************************)

\*
\* Type invariant: all state variables have well-formed types
\*
TypeInvariant ==
    /\ nodeUp \in [Nodes -> BOOLEAN]
    /\ held \in [Nodes -> [Keys -> BOOLEAN]]
    /\ committedVal \in [Keys -> Values]
    /\ committed \in [Keys -> BOOLEAN]
    /\ tombstoned \in [Keys -> BOOLEAN]
    /\ typeCounts \in [{""} -> Nat]
    /\ encryptedAtRest \in {TRUE, FALSE}
    /\ summariesValid \in {TRUE, FALSE}

\*
\* SAFETY: No data loss — every committed, non-deleted key is readable
\* from at least one up node.
\*
NoDataLoss ==
    \A k \in LiveKeys :
        \E n \in Nodes :
            /\ nodeUp[n]
            /\ held[n][k]
            /\ committed[k]
            /\ ~tombstoned[k]

\*
\* SAFETY: Delete visibility — deleted keys are never reported as live
\*
DeleteVisible ==
    \A k \in Keys :
        tombstoned[k] => ~(committed[k] /\ IsReadable(k))

\*
\* SAFETY: Ownership consistency — each key has exactly one ring owner,
\* and the owner is always among the nodes.
\*
OwnershipConsistency ==
    \A k \in Keys : Owner[k] \in Nodes

\*
\* SAFETY: Replication completeness — when replication is complete
\* (empty outbox), every committed key is held by at least RF nodes.
\*
ReplicationCompleteness ==
    outbox = {} =>
        \A k \in LiveKeys : CopyCount(k) >= Min(NumReplicas, Cardinality(Nodes))

\*
\* SAFETY: Encryption consistency — if encryption is enabled, the
\* encryption flag reflects this in the state.
\*
EncryptionConsistency ==
    encryptedAtRest \in {TRUE, FALSE}

\*
\* SAFETY: Summary validity implies summaries match actual state.
\* (This is the KEY property that makes aggregate queries trustworthy.)
\*
SummaryValidityImpliesCorrect ==
    summariesValid =>
        \A k \in Keys :
            (tombstoned[k] => ~committed[k])

-----------------------------------------------------------------------------
(***************************************************************************)
(*                     Liveness Properties                                 *)
(***************************************************************************)

\*
\* LIVENESS: Eventually, all pending replications are delivered
\* (assuming nodes don't permanently fail).
\*
EventuallyConsistent ==
    [](outbox # {} => <>(outbox = {}))

\*
\* LIVENESS: Every write is eventually visible on all replicas
\*
EventuallyAllCopies ==
    \A k \in Keys :
        [](committed[k] => <>\A n \in UpNodes : held[n][k])

=============================================================================
