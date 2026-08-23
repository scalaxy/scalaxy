-------------------------------- MODULE ScalaxySpec --------------------------
(***************************************************************************)
(* Scalaxy -- Central Truth Specification (v2)                             *)
(*                                                                         *)
(* Storage correctness + reliability + security for S3-backed graph        *)
(* storage, modeled the way the system actually works:                     *)
(*                                                                         *)
(*   write path : local durable copy + durable outbox entry                *)
(*   replicate  : background shipper copies to a peer asynchronously       *)
(*   delete     : immediate tombstone; key invisible and permanent         *)
(*   crash      : node stops; durable state (held, outbox) survives        *)
(*                                                                         *)
(* Machine-verified with TLC. See README.md for commands and results;      *)
(* see CRITIQUE.md for what changed since v1 and what is abstracted.       *)
(***************************************************************************)
EXTENDS Naturals, FiniteSets

CONSTANTS
  \* Set of nodes.
  Nodes,
  \* Set of keys.
  Keys,
  \* Replication factor: number of durable copies per live key.
  Rf

ASSUME RfAssume == Rf >= 1 /\ Rf <= Cardinality(Nodes)

VARIABLES
  \* nodeUp[n]: TRUE iff node n is running.
  nodeUp,
  \* encOn[n]: TRUE iff node n encrypts data at rest.
  encOn,
  \* held[n][k]: TRUE iff node n holds a durable copy of key k.
  held,
  \* outbox[n][k]: TRUE iff node n still owes peers a replica of k
  \* (durable; survives crashes).
  outbox,
  \* live[k]: TRUE iff key k is committed and client-visible.
  live,
  \* tombstoned[k]: TRUE iff key k was deleted (permanent).
  tombstoned

vars == <<nodeUp, encOn, held, outbox, live, tombstoned>>

NodeSubset == SUBSET Nodes

-----------------------------------------------------------------------------

Init ==
  /\ nodeUp     = [n \in Nodes |-> TRUE]
  /\ encOn      = [n \in Nodes |-> TRUE]
  /\ held       = [n \in Nodes |-> [k \in Keys |-> FALSE]]
  /\ outbox     = [n \in Nodes |-> [k \in Keys |-> FALSE]]
  /\ live       = [k \in Keys |-> FALSE]
  /\ tombstoned = [k \in Keys |-> FALSE]

-----------------------------------------------------------------------------
\*
\* WRITE RULE (central truth): a write becomes visible once its FIRST
\* durable encrypted copy exists; reaching RF copies is asynchronous.
\* Deleted keys can never be written again.
\*
ClientWrite(k, w) ==
  /\ ~live[k]
  /\ ~tombstoned[k]
  /\ nodeUp[w]
  /\ encOn[w]
  /\ held'      = [held EXCEPT ![w][k] = TRUE]
  /\ outbox'    = [outbox EXCEPT ![w][k] = (Rf > 1)]
  /\ live'      = [live EXCEPT ![k] = TRUE]
  /\ UNCHANGED <<nodeUp, encOn, tombstoned>>

\* REPLICATION RULE (central truth): the shipper copies an under-
\* replicated key from any holder to any up, encrypted node that lacks
\* a copy, and retires the source's outbox entry once Rf copies exist.
Holders(k) == {n \in Nodes : held[n][k]}

\* A key is fully replicated once Rf durable copies exist.
Replicated(k) == Cardinality(Holders(k)) >= Rf

ShipReplica(k, src, dst) ==
  /\ held[src][k]
  /\ outbox[src][k]
  /\ ~held[dst][k]
  /\ dst # src
  /\ ~tombstoned[k]
  /\ nodeUp[src]
  /\ nodeUp[dst]
  /\ encOn[dst]
  /\ held'      = [held EXCEPT ![dst][k] = TRUE]
  /\ outbox'    =
        [outbox EXCEPT ![src][k] = (Cardinality(Holders(k)) + 1 < Rf)]
  /\ UNCHANGED <<nodeUp, encOn, live, tombstoned>>

\* DELETE RULE (central truth): the tombstone is set immediately; the
\* key becomes invisible atomically and can never be written again.
\* ABSTRACTION NOTE: per-node visibility propagation delay is folded
\* into this atomic flag -- see CRITIQUE.md issue 4.
ClientDelete(k) ==
  /\ live[k]
  /\ ~tombstoned[k]
  /\ live'       = [live EXCEPT ![k] = FALSE]
  /\ tombstoned' = [tombstoned EXCEPT ![k] = TRUE]
  /\ UNCHANGED <<nodeUp, encOn, held, outbox>>

\* Crash: node stops serving. Durable state (held, outbox, encryption
\* config) survives on disk.
CrashNode(n) ==
  /\ nodeUp[n]
  /\ nodeUp'    = [nodeUp EXCEPT ![n] = FALSE]
  /\ UNCHANGED <<encOn, held, outbox, live, tombstoned>>

\* Recover: restart restores all durable state intact.
RecoverNode(n) ==
  /\ ~nodeUp[n]
  /\ nodeUp'    = [nodeUp EXCEPT ![n] = TRUE]
  /\ UNCHANGED <<encOn, held, outbox, live, tombstoned>>

\* SECURITY RULE: encryption at rest may be toggled on a node only while
\* it holds no data. Plaintext must never exist on any disk.
Drained(n) == \A k \in Keys : ~held[n][k]

DisableEncryption(n) ==
  /\ encOn[n]
  /\ Drained(n)
  /\ encOn'     = [encOn EXCEPT ![n] = FALSE]
  /\ UNCHANGED <<nodeUp, held, outbox, live, tombstoned>>

EnableEncryption(n) ==
  /\ ~encOn[n]
  /\ Drained(n)
  /\ encOn'     = [encOn EXCEPT ![n] = TRUE]
  /\ UNCHANGED <<nodeUp, held, outbox, live, tombstoned>>

-----------------------------------------------------------------------------

Next ==
  \/ \E k \in Keys, w \in Nodes            : ClientWrite(k, w)
  \/ \E k \in Keys, s \in Nodes, d \in Nodes : ShipReplica(k, s, d)
  \/ \E k \in Keys                      : ClientDelete(k)
  \/ \E n \in Nodes                     : CrashNode(n)
  \/ \E n \in Nodes                     : RecoverNode(n)
  \/ \E n \in Nodes                     : DisableEncryption(n)
  \/ \E n \in Nodes                     : EnableEncryption(n)

\* FAIRNESS (central truth): the shipper and recovery are eventually
\* scheduled whenever continuously enabled. Without these conjuncts a
\* scheduler could starve replication forever.
Fairness ==
  /\ \A k \in Keys, s \in Nodes, d \in Nodes :
        WF_vars(ShipReplica(k, s, d))
  /\ \A n \in Nodes : WF_vars(RecoverNode(n))

Spec ==
  Init /\ [][Next]_vars /\ Fairness

-----------------------------------------------------------------------------
(***************************************************************************)
(* SAFETY INVARIANTS                                                       *)
(***************************************************************************)

TypeOK ==
  /\ nodeUp     \in [Nodes -> BOOLEAN]
  /\ encOn      \in [Nodes -> BOOLEAN]
  /\ held       \in [Nodes -> [Keys -> BOOLEAN]]
  /\ outbox     \in [Nodes -> [Keys -> BOOLEAN]]
  /\ live       \in [Keys -> BOOLEAN]
  /\ tombstoned \in [Keys -> BOOLEAN]

\* SAFETY: every visible key has at least one durable copy at all times,
\* including inside the single-copy window before async replication.
DataIntegrity ==
  \A k \in Keys :
    live[k] => \E n \in Nodes : held[n][k]

\* SAFETY: deleted keys never appear live.
DeleteVisible ==
  \A k \in Keys :
    ~(live[k] /\ tombstoned[k])

\* SECURITY: no key is ever durably stored on an unencrypted node.
EncryptionAtRest ==
  \A n \in Nodes, k \in Keys :
    held[n][k] => encOn[n]

\* RELIABILITY: once every live key is fully replicated (>= Rf copies on
\* distinct nodes), any single-node crash costs zero availability.
\* NOTE: BEFORE convergence a crash of the sole holder CAN cost
\* temporary availability -- TLC falsified the stronger v1 claim
\* ("always safe under single failure") in one step. The durable copy
\* survives and service resumes at recovery; see CRITIQUE.md.
AvailabilityUnderSingleFailureAfterReplication ==
  (
    (\A k \in Keys : live[k] => Replicated(k))
    /\ Cardinality({n \in Nodes : ~nodeUp[n]}) <= 1
  ) =>
    (\A k \in Keys :
      live[k] => \E h \in Nodes : nodeUp[h] /\ held[h][k])

-----------------------------------------------------------------------------
(***************************************************************************)
(* LIVENESS (temporal properties, checked under Spec's fairness)           *)
(***************************************************************************)

\* LIVENESS: every visible key EVENTUALLY becomes readable again under
\* fair scheduling -- a crashed sole holder recovers (WF on RecoverNode)
\* and replication gives a second holder.
ServiceRestored ==
  \A k \in Keys :
    live[k] ~> (\E h \in Nodes : nodeUp[h] /\ held[h][k])

\* RELIABILITY: crashes never destroy durable copies.
RecoveryRestoresService ==
  \A k \in Keys :
    live[k] => \E h \in Nodes : held[h][k]


\* LIVENESS: if the cluster becomes stable-up, every live key eventually
\* reaches full replication. This is the promise the durable outbox +
\* fair shipper makes: under-replicated data converges.
ReplicationConverges ==
  [](\A n \in Nodes : nodeUp[n]) =>
      <>(\A k \in Keys : live[k] => Replicated(k))

=============================================================================
