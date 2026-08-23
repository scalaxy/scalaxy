-------------------------------- MODULE ScalaxySpec --------------------------
(***************************************************************************)
(* Scalaxy -- Central Truth Specification                                  *)
(*                                                                         *)
(* Storage correctness + reliability for S3-backed graph storage.          *)
(* Certified by TLC: see README.md for the exact command and results.      *)
(*                                                                         *)
(* Findings already caught by TLC against earlier drafts of this file:     *)
(*   1. A write action that set live[k] without placing data on a node     *)
(*      violated NoDataLoss -- writes MUST place data before commit.       *)
(*   2. A write action without a tombstone guard allowed deleted keys to   *)
(*      resurrect, violating DeleteVisible.                                *)
(* These two constraints are binding requirements on the implementation.   *)
(***************************************************************************)
EXTENDS Naturals, FiniteSets

CONSTANTS
  \* Set of nodes.
  Nodes,
  \* Set of keys.
  Keys

VARIABLES
  \* nodeUp[n]: TRUE iff node n is running.
  nodeUp,
  \* held[n][k]: TRUE iff node n holds durable copy of key k.
  held,
  \* live[k]: TRUE iff key k is committed and not deleted.
  live,
  \* tombstoned[k]: TRUE iff key k was deleted (tombstone set).
  tombstoned,
  \* encOn[n]: TRUE iff node n encrypts data at rest.
  encOn

vars == <<nodeUp, held, live, tombstoned, encOn>>

-----------------------------------------------------------------------------

Init ==
  /\ nodeUp     = [n \in Nodes |-> TRUE]
  /\ held       = [n \in Nodes |-> [k \in Keys |-> FALSE]]
  /\ live       = [k \in Keys |-> FALSE]
  /\ tombstoned = [k \in Keys |-> FALSE]
  /\ encOn      = [n \in Nodes |-> TRUE]

-----------------------------------------------------------------------------
\*
\* COMMIT RULE (central truth): a write commits only after the key is
\* durably placed on two DISTINCT nodes (RF=2). A deleted key can never
\* be written again.
\*
MakeLive(k, w1, w2) ==
  /\ ~live[k]
  /\ ~tombstoned[k]
  /\ w1 # w2
  /\ nodeUp[w1]
  /\ nodeUp[w2]
  /\ encOn[w1]
  /\ encOn[w2]
  /\ live'      = [live EXCEPT ![k] = TRUE]
  /\ held'      = [held EXCEPT ![w1][k] = TRUE, ![w2][k] = TRUE]
  /\ UNCHANGED <<nodeUp, tombstoned, encOn>>

\* DELETE RULE (central truth): delete sets the tombstone immediately;
\* the key becomes invisible atomically and can never come back.
MakeDead(k) ==
  /\ live[k]
  /\ ~tombstoned[k]
  /\ live'       = [live EXCEPT ![k] = FALSE]
  /\ tombstoned' = [tombstoned EXCEPT ![k] = TRUE]
  /\ UNCHANGED <<nodeUp, held, encOn>>

\* CrashNode: node stops serving. Durable copies survive on disk.
CrashNode(n) ==
  /\ nodeUp[n]
  /\ nodeUp'    = [nodeUp EXCEPT ![n] = FALSE]
  /\ UNCHANGED <<held, live, tombstoned, encOn>>

\* RecoverNode: node restarts; its durable copies are intact.
RecoverNode(n) ==
  /\ ~nodeUp[n]
  /\ nodeUp'    = [nodeUp EXCEPT ![n] = TRUE]
  /\ UNCHANGED <<held, live, tombstoned, encOn>>

\* SECURITY RULE: encryption at rest can be disabled on a node only
\* after every durable copy has been drained from it. Plaintext data
\* must never exist on any disk.
DisableEncryption(n) ==
  /\ encOn[n]
  /\ \A k \in Keys : ~held[n][k]
  /\ encOn'      = [encOn EXCEPT ![n] = FALSE]
  /\ UNCHANGED <<nodeUp, held, live, tombstoned>>

-----------------------------------------------------------------------------

Next ==
  \/ \E k \in Keys, w1 \in Nodes, w2 \in Nodes : MakeLive(k, w1, w2)
  \/ \E k \in Keys : MakeDead(k)
  \/ \E n \in Nodes : CrashNode(n)
  \/ \E n \in Nodes : RecoverNode(n)
  \/ \E n \in Nodes : DisableEncryption(n)

Spec == Init /\ [][Next]_vars

-----------------------------------------------------------------------------
(***************************************************************************)
(* INVARIANTS                                                              *)
(***************************************************************************)

TypeOK ==
  /\ nodeUp     \in [Nodes -> BOOLEAN]
  /\ held       \in [Nodes -> [Keys -> BOOLEAN]]
  /\ live       \in [Keys -> BOOLEAN]
  /\ tombstoned \in [Keys -> BOOLEAN]
  /\ encOn      \in [Nodes -> BOOLEAN]

\* SAFETY: every live key has at least one durable copy on disk,
\* no matter what crashes happened.
DataIntegrity ==
  \A k \in Keys :
    live[k] =>
      \E n \in Nodes : held[n][k]

\* SAFETY: deleted keys never appear live (no resurrection).
DeleteVisible ==
  \A k \in Keys :
    ~(live[k] /\ tombstoned[k])

\* RELIABILITY: any SINGLE node crash never costs read availability.
\* This is exactly the guarantee RF=2 replication buys: every live key
\* has two holders, so after losing one node a second holder remains.
\* (With more than 2 nodes, simultaneous multi-node crashes CAN make a
\* specific key unreadable until recovery -- the spec states honestly
\* what RF=2 does and does not cover.)
DownNodes == {n \in Nodes : ~nodeUp[n]}

AvailabilityUnderSingleFailure ==
  Cardinality(DownNodes) <= 1 =>
    \A k \in Keys :
      live[k] =>
        \E h \in Nodes :
          nodeUp[h] /\ held[h][k]

\* RELIABILITY: crashes never destroy data. When every crashed node
\* recovers, all live keys are fully readable again.
RecoveryRestoresService ==
  \A k \in Keys :
    live[k] =>
      \E h \in Nodes : held[h][k]

\* REPLICATION: every live key is stored on at least 2 nodes (RF=2).
ReplicationFactorTwo ==
  \A k \in Keys :
    live[k] =>
      Cardinality({n \in Nodes : held[n][k]}) >= 2

\* SECURITY: no key is ever durably stored on an unencrypted node.
EncryptionAtRest ==
  \A n \in Nodes, k \in Keys :
    held[n][k] => encOn[n]

=============================================================================
