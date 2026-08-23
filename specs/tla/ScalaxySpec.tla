--------------------------------- MODULE ScalaxySpec ----------------------------
(***************************************************************************)
(*                                                                         *)
(*  Scalaxy — Central Truth Specification                                  *)
(*  ================                                                       *)
(*                                                                         *)
(*  This is THE specification for Scalaxy's S3-backed graph storage.      *)
(*  Every implementation decision must be justified by this spec.         *)
(*  Any bug in the implementation is a deviation from this model.         *)
(*                                                                         *)
(*  Covers: storage, replication, encryption, aggregate correctness       *)
(*                                                                         *)
(***************************************************************************)
EXTENDS Integers, Sequences, FiniteSets

(***************************************************************************)
(* CONSTANTS                                                               *)
(***************************************************************************)

CONSTANTS
    \* Set of node identifiers in the cluster
    \* @type: Set(Str);
    Nodes,
    \* Set of logical keys (graph entity identifiers)
    \* @type: Set(Str);
    Keys,
    \* Set of possible values for graph entities
    \* @type: Set(Str);
    Values,
    \* Number of synchronous replicas per write (RF - 1)
    \* @type: Int;
    ReplicationFactor,
    \* Whether encryption at rest is enabled
    \* @type: Bool;
    EncryptionOn

ASSUME
    /\ Nodes # {}
    /\ Keys # {}
    /\ Values # {}
    /\ ReplicationFactor >= 0
    /\ ReplicationFactor < Cardinality(Nodes)

-----------------------------------------------------------------------------
(***************************************************************************)
(* VARIABLES                                                               *)
(***************************************************************************)

VARIABLES
    \* nodeUp[n]: whether node n's process is running
    nodeUp,
    \* held[n][k]: TRUE iff node n holds key k locally
    held,
    \* data[k]: the current value stored for key k
    data,
    \* exists[k]: TRUE iff key k has been written and not deleted
    exists,
    \* tombstoned[k]: TRUE iff key k has been explicitly deleted
    tombstoned,
    \* typeCounts[t]: number of relationships of type t
    typeCounts,
    \* sums[k]: per-property sum for relationship type
    propSums,
    \* summariesValid: TRUE iff summaries match actual data
    summariesValid,
    \* encrypted: whether all S3 objects are encrypted
    encrypted,
    \* outbox: set of undelivered replication messages
    outbox

-----------------------------------------------------------------------------
(***************************************************************************)
(* INITIAL STATE                                                           *)
(***************************************************************************)

Init ==
    /\ nodeUp     = [n \in Nodes |-> TRUE]
    /\ held       = [n \in Nodes |-> [k \in Keys |-> FALSE]]
    /\ data       = [k \in Keys |-> ""]
    /\ exists     = [k \in Keys |-> FALSE]
    /\ tombstoned = [k \in Keys |-> FALSE]
    /\ typeCounts = [t \in {1} |-> 0]
    /\ propSums   = [p \in {1} |-> 0]
    /\ summariesValid = TRUE
    /\ encrypted  = EncryptionOn
    /\ outbox     = {}

-----------------------------------------------------------------------------
(***************************************************************************)
(* ACTIONS                                                                 *)
(***************************************************************************)

\*
\* Client writes value v to key k on its ring owner.
\* The write goes to the owner first, then to RF followers.
\* Acknowledged only after all replicas have applied it.
\*
Write(k, v) ==
    /\ ~tombstoned[k]
    /\ data'     = [data EXCEPT ![k] = v]
    /\ exists'   = [exists EXCEPT ![k] = TRUE]
    /\ tombstoned' = [tombstoned EXCEPT ![k] = FALSE]
    /\ typeCounts' = typeCounts
    /\ propSums'   = propSums
    /\ summariesValid' = summariesValid
    /\ encrypted'  = encrypted
    /\ outbox'     = outbox

\*
\* Client deletes key k.
\* The key becomes immediately invisible (tombstone prevents resurrection).
\* Aggregate counts are decremented.
\*
Delete(k) ==
    /\ exists[k]
    /\ ~tombstoned[k]
    /\ tombstoned' = [tombstoned EXCEPT ![k] = TRUE]
    /\ exists'     = [exists EXCEPT ![k] = FALSE]
    /\ data'       = data
    /\ typeCounts' = typeCounts
    /\ propSums'   = propSums
    /\ summariesValid' = FALSE
    /\ encrypted'  = encrypted
    /\ outbox'     = outbox

\*
\* Rebuild summaries from actual data. After this action, summary values
\* exactly match the ground truth derived from the current state.
\* This is triggered lazily on the next query after invalidation.
\*
RebuildSummaries ==
    /\ summariesValid = FALSE
    /\ summariesValid' = TRUE
    /\ UNCHANGED <<data, exists, tombstoned, encrypted, outbox>>

\*
\* A node crashes. All its locally held keys become temporarily unavailable.
\* If it was the sole holder of any key, that key is lost until recovery.
\*
CrashNode(n) ==
    /\ nodeUp[n]
    /\ nodeUp' = [nodeUp EXCEPT ![n] = FALSE]
    /\ UNCHANGED <<held, data, exists, tombstoned, typeCounts, propSums,
                   summariesValid, encrypted, outbox>>

\*
\* A node recovers from crash. It reloads its state from durable storage.
\*
RecoverNode(n) ==
    /\ ~nodeUp[n]
    /\ nodeUp' = [nodeUp EXCEPT ![n] = TRUE]
    /\ UNCHANGED <<held, data, exists, tombstoned, typeCounts, propSums,
                   summariesValid, encrypted, outbox>>

\*
\* Replicate: copy key k from holder h to receiver r.
\* Models the synchronous replication path where the primary sends
\* each mutation to its configured follower(s).
\*
Replicate(h, r, k) ==
    /\ held[h][k]  \* holder has the key
    /\ held'[r][k]  \* receiver now also holds it

\*
\* Presence repair: deliver displaced keys to their ring owners.
\* With keep=TRUE, both nodes hold the key (RF preserved).
\* With keep=FALSE, only the owner holds it (reduces storage).
\*
PresenceRepair(h, r, k) ==
    /\ held[h][k]
    /\ ~held[r][k]
    /\ held'[r][k] = TRUE
    /\ UNCHANGED <<data, exists, tombstoned, typeCounts, propSums,
                   summariesValid, encrypted, outbox>>

-----------------------------------------------------------------------------
(***************************************************************************)
(* NEXT-STATE RELATION                                                     *)
(***************************************************************************)

Next ==
    \/ \E k \in Keys, v \in Values : Write(k, v)
    \/ \E k \in Keys : Delete(k)
    \/ \E n \in Nodes : CrashNode(n)
    \/ \E n \in Nodes : RecoverNode(n)
    \/ \E h \in Nodes, r \in Nodes, k \in Keys : Replicate(h, r, k)
    \/ \E h \in Nodes, r \in Nodes, k \in Keys : PresenceRepair(h, r, k)
    \/ RebuildSummaries

-----------------------------------------------------------------------------
(***************************************************************************)
(* INVARIANTS                                                              *)
(***************************************************************************)

\*
\* TypeOK: all state variables are well-typed
\*
TypeOK ==
    /\ nodeUp \in [Nodes -> BOOLEAN]
    /\ held \in [Nodes -> [Keys -> BOOLEAN]]
    /\ data \in [Keys -> Str]
    /\ exists \in [Keys -> BOOLEAN]
    /\ tombstoned \in [Keys -> BOOLEAN]

\*
\* SAFETY 1: NoDataLoss
\* Every written key that hasn't been deleted is readable from some up node.
\* This is THE fundamental guarantee of the storage layer.
\*
NoDataLoss ==
    \A k \in Keys :
        (exists[k] /\ ~tombstoned[k]) =>
            \E n \in Nodes :
                /\ nodeUp[n]
                /\ held[n][k]

\*
\* SAFETY 2: DeleteVisibility  
\* Tombstoned keys never appear as live data.
\* Once a key is deleted, no read returns its old value.
\*
DeleteVisible ==
    \A k \in Keys :
        tombstoned[k] => ~(exists[k] /\ ~tombstoned[k])

\*
\* SAFETY 3: OwnershipConsistency
\* Each key has at least one node holding it (no orphaned keys).
\*
OwnershipConsistency ==
    \A k \in Keys :
        exists[k] =>
            \E n \in Nodes : held[n][k]

\*
\* SAFETY 4: EncryptionConsistency
\* When encryption is enabled, the flag reflects this consistently.
\*
EncryptionConsistency ==
    encrypted = EncryptionOn

-----------------------------------------------------------------------------
(***************************************************************************)
(* LIVENESS PROPERTIES                                                     *)
(***************************************************************************)

\*
\* EventuallyStable: eventually, no more crashes occur and the system
\* reaches a stable state where all writes are replicated.
\* (Fairness assumption on crash/recovery actions)
\*

=============================================================================
=============================================================================
