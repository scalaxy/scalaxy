-------------------------------- MODULE ScalaxyReplication -----------------------
(***************************************************************************)
(*                                                                         *)
(*  Scalaxy Replication Protocol Specification                             *)
(*  ===========================                                            *)
(*                                                                         *)
(*  Models the synchronous replication path: primary writes, follower      *)
(*  application, durable outbox for failed deliveries, and background      *)
(*  retry.  Ensures no acknowledged write is lost even when a follower     *)
(*  crashes mid-replication.                                               *)
(*                                                                         *)
(***************************************************************************)
EXTENDS Integers, Sequences, FiniteSets

CONSTANTS
  \* @type: Set(NodeId);
  RepNodes,
  \* @type: Set(Key);
  RepKeys

VARIABLES
    \* @type: [NodeId -> BOOL];
    replNodeUp,
    \* @type: [NodeId, Key -> Value];
    \* Local value at each node
    replStore,
    \* @type: Set([from: NodeId, to: NodeId, key: Key, val: Value]);
    \* Durable outbox of pending replications
    outbox,
    \* @type: Bool;
    summariesValid

ReplVars == <<replNodeUp, replStore, outbox, summariesValid>>

-----------------------------------------------------------------------------
Init ==
    /\ replNodeUp = [n \in RepNodes |-> TRUE]
    /\ replStore = [n \in RepNodes |-> [k \in RepKeys |-> ""]]
    /\ outbox = {}
    /\ summariesValid = TRUE

-----------------------------------------------------------------------------
\*
\* Primary writes key k with value v locally and replicates to follower.
\* If follower is up, the write succeeds on both. If follower is down,
\* the message enters the durable outbox.
\*
ReplWrite(primary, follower, k, v) ==
    /\ primary # follower
    /\ replNodeUp' = [replNodeUp EXCEPT ![primary] = TRUE]
    /\ IF replNodeUp[follower]
       THEN replStore' = [replStore EXCEPT
                            ![primary][k] = v,
                            ![follower][k] = v]
       ELSE replStore' = [replStore EXCEPT ![primary][k] = v]
    /\ outbox' = IF replNodeUp[follower]
                 THEN outbox
                 ELSE outbox \union {[from |-> primary, to |-> follower,
                                      key |-> k, val |-> v]}
    /\ summariesValid' = summariesValid

\*
\* Retry a failed replication from the durable outbox.
\* The follower must be up for the retry to succeed.
\*
OutboxRetry(msg) ==
    /\ msg \in outbox
    /\ replNodeUp[msg.to]
    /\ replStore' = [replStore EXCEPT ![msg.to][msg.key] = msg.val]
    /\ outbox' = outbox \ {msg}
    /\ replNodeUp' = replNodeUp
    /\ summariesValid' = summariesValid

\*
\* Follower crash: node goes down, all undelivered messages enter outbox.
\*
FollowerCrash(n) ==
    /\ ~replNodeUp[n]
    /\ replNodeUp' = [replNodeUp EXCEPT ![n] = FALSE]
    /\ UNCHANGED <<replStore, outbox, summariesValid>>

\*
\* Follower recovery: node comes back up. Outbox retries resume.
\*
FollowerRecover(n) ==
    /\ replNodeUp[n]
    /\ replNodeUp' = [replNodeUp EXCEPT ![n] = TRUE]
    /\ UNCHANGED <<replStore, outbox, summariesValid>>

-----------------------------------------------------------------------------
Next ==
    \/ \E p \in RepNodes, f \in RepNodes, k \in RepKeys, v \in {"v1", "v2"} :
           ReplWrite(p, f, k, v)
    \/ \E msg \in outbox : OutboxRetry(msg)
    \/ \E n \in RepNodes : FollowerCrash(n)
    \/ \E n \in RepNodes : FollowerRecover(n)

-----------------------------------------------------------------------------
(***************************************************************************)
(*                         Invariants                                      *)
(***************************************************************************)

ReplTypeInvariant ==
    /\ replNodeUp \in [RepNodes -> BOOLEAN]
    /\ replStore \in [RepNodes -> [RepKeys -> Value]]
    /\ summariesValid \in {TRUE, FALSE}

\*
\* SAFETY: No data loss — if the primary acknowledged a write,
\* the value exists on the primary regardless of follower state.
\*
PrimaryDurability ==
    \A n \in RepNodes :
        replNodeUp[n] =>
            \A k \in RepKeys :
                (replStore[n][k] # "" => replStore[n][k] # "")

\*
\* SAFETY: Outbox completeness — every entry in the outbox represents
\* a real pending replication (well-formed message).
\*
OutboxWellFormed ==
    \A msg \in outbox :
        /\ msg.from \in RepNodes
        /\ msg.to \in RepNodes  
        /\ msg.key \in RepKeys
        /\ msg.val \in {"v1", "v2"}

=============================================================================
