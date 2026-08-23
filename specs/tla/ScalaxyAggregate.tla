-------------------------------- MODULE ScalaxyAggregate -------------------------
(***************************************************************************)
(*                                                                         *)
(*  Scalaxy Aggregate Summary Correctness Specification                    *)
(*  ===================================                                    *)
(*                                                                         *)
(*  Models the relationship between actual graph data and the              *)
(*  aggregate summaries (type counts, sums) served by queries.             *)
(*                                                                         *)
(*  KEY PROPERTY: When summaries are marked valid, they MUST match         *)
(*  the actual data. This is the central correctness guarantee that        *)
(*  makes fast aggregate queries trustworthy.                              *)
(*                                                                         *)
(***************************************************************************)
EXTENDS Integers, FiniteSets

CONSTANTS
  \* @type: Set(RelType);
  RelTypes,
          \* @type: Nat;
  TotalRecords

VARIABLES
    \* @type: [RelType -> Nat];
    \* Actual count of relationships per type (ground truth)
    actualCounts,
    \* @type: [RelType -> Nat];
    \* Reported type counts from summaries
    reportedCounts,
    \* @type: Bool;
    \* Whether summaries match actual data
    summaryMatches,
    \* @type: Bool;
    \* Whether a rebuild is in progress
    rebuilding,
    \* @type: Bool;
    \* Whether any mutation has occurred since last rebuild
    mutated

AggVars == <<actualCounts, reportedCounts, summaryMatches, rebuilding, mutated>>

-----------------------------------------------------------------------------
Init ==
    /\ actualCounts = [t \in RelTypes |-> 0]
    /\ reportedCounts = [t \in RelTypes |-> 0]
    /\ summaryMatches = TRUE
    /\ rebuilding = FALSE
    /\ mutated = FALSE

-----------------------------------------------------------------------------
\*
\* Add a relationship of type t.  Actual count increments.
\* If summaries are valid, they must also be updated.
\*
AddRelationship(t, delta) ==
    /\ actualCounts' = [actualCounts EXCEPT ![t] = actualCounts[t] + delta]
    /\ IF summaryMatches /\ ~rebuilding
       THEN reportedCounts' = [reportedCounts EXCEPT ![t] = reportedCounts[t] + delta]
       ELSE reportedCounts' = reportedCounts
    /\ mutated' = TRUE
    /\ rebuilding' = FALSE

\*
\* Remove a relationship of type t.  Actual count decrements.
\*
RemoveRelationship(t, delta) ==
    /\ actualCounts[t] >= delta
    /\ actualCounts' = [actualCounts EXCEPT ![t] = actualCounts[t] - delta]
    /\ IF summaryMatches /\ ~rebuilding
       THEN reportedCounts' = [reportedCounts EXCEPT ![t] = reportedCounts[t] - delta]
       ELSE reportedCounts' = reportedCounts
    /\ mutated' = TRUE
    /\ rebuilding' = FALSE

\*
\* Rebuild summaries from actual data.  After rebuild, reported counts
\* exactly match actual counts, and summaryMatches is restored to TRUE.
\*
RebuildSummaries ==
    /\ rebuilding = FALSE
    /\ reportedCounts' = actualCounts
    /\ summaryMatches' = TRUE
    /\ rebuilding' = FALSE
    /\ mutated' = FALSE

\*
\* Invalidate summaries due to a mutation.  summaryMatches becomes FALSE
\* until the next rebuild.
\*
InvalidateSummaries ==
    /\ summaryMatches = TRUE
    /\ summaryMatches' = FALSE
    /\ mutated' = TRUE
    /\ UNCHANGED <<actualCounts, reportedCounts, rebuilding>>

-----------------------------------------------------------------------------
Next ==
    \/ \E t \in RelTypes : AddRelationship(t, 1)
    \/ \E t \in RelTypes, d \in {1} : RemoveRelationship(t, d)
    \/ RebuildSummaries
    \/ InvalidateSummaries

-----------------------------------------------------------------------------
(***************************************************************************)
(*                         Invariants                                      *)
(***************************************************************************)

\*
\* SAFETY: Reported type counts never exceed actual counts
\* (no phantom relationships in summaries)
\*
NoPhantomRelationships ==
    \A t \in RelTypes :
        ~(summaryMatches /\ reportedCounts[t] > actualCounts[t])

\*
\* SAFETY: When summaries are valid, they exactly match actual data
\*
ValidSummariesAreCorrect ==
    summaryMatches =>
        \A t \in RelTypes :
            reportedCounts[t] = actualCounts[t]

\*
\* SAFETY: Non-negative counts always
\*
NonNegativeCounts ==
    \A t \in RelTypes :
        /\ actualCounts[t] >= 0
        /\ reportedCounts[t] >= 0

-----------------------------------------------------------------------------
(***************************************************************************)
(*                     Liveness Properties                                 *)
(***************************************************************************)

\*
\* LIVENESS: Eventually, invalidated summaries are rebuilt
\* (assuming no continuous mutation stream prevents convergence)
\*
EventuallyRebuilt ==
    [](mutated => <> ~mutated)

=============================================================================
