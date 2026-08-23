# Scalaxy TLA+ Specifications

This directory contains formal specifications (TLA+) that serve as the
**central truth** for how Scalaxy must be implemented.  These specifications
define the correctness properties that the implementation MUST satisfy.

## Modules

| Module | Covers | Key Properties |
|--------|--------|----------------|
| `ScalaxyStorage.tla` | Core storage: writes, deletes, replication, node crashes | NoDataLoss, DeleteVisible, ReplicationCompleteness |
| `ScalaxyReplication.tla` | Replication protocol, durable outbox, failover | PrimaryDurability, OutboxWellFormed |
| `ScalaxyEncryption.tla` | Encryption at rest (SCX1 format) | CiphertextDiffers, EncryptedKeysHaveCiphertext |
| `ScalaxyAggregate.tla` | Aggregate summary correctness | ValidSummariesAreCorrect, NoPhantomRelationships |

## Running TLC Model Checker

```sh
# Download TLA+ tools
wget https://github.com/tlaplus/tlaplus/releases/latest/download/tla2tools.jar

# Run TLC on a spec with a config file
java -cp tla2tools.jar tlc2.TLC -config ScalaxyStorage.cfg ScalaxyStorage.tla
```

## Specification Structure

Each module defines:

1. **CONSTANTS**: Parameters (nodes, keys, values) — instantiated per model
2. **VARIABLES**: State variables that change over time
3. **Init**: The initial state predicate
4. **Next**: The next-state relation (all possible transitions)
5. **Invariants**: Safety properties that must hold in ALL reachable states

### Key Invariants

#### Storage (`ScalaxyStorage.tla`)

- **NoDataLoss**: Every committed, non-deleted key is readable from at least one up node.
- **DeleteVisible**: Deleted keys are never returned as live data.
- **OwnershipConsistency**: Each key has exactly one ring owner.
- **ReplicationCompleteness**: When outbox is empty, every committed key exists on ≥ RF nodes.

#### Replication (`ScalaxyReplication.tla`)

- **PrimaryDurability**: Acknowledged writes survive follower crashes.
- **OutboxWellFormed**: All outbox entries represent real pending replications.

#### Encryption (`ScalaxyEncryption.tla`)

- **CiphertextDiffers**: Ciphertext is different from plaintext.
- **EncryptedKeysHaveCiphertext**: All encrypted keys have non-empty ciphertext.

#### Aggregates (`ScalaxyAggregate.tla`)

- **ValidSummariesAreCorrect**: When summaries are marked valid, they exactly match actual data.
- **NoPhantomRelationships**: Reported counts never exceed actual counts.

## Implementation Conformance

The Scalaxy implementation in `src/` must satisfy these specifications.
Any deviation from the specified behavior is a bug.

Key implementation files and their corresponding spec sections:

| Implementation file | Spec module |
|---|---|
| `src/s3.lisp` | ScalaxyStorage |
| `src/storage.lisp` | ScalaxyStorage |
| `src/storage-plugins.lisp` | ScalaxyEncryption |
| `src/gateway.lisp` | ScalaxyAggregate |

## License

MIT — same as the main Scalaxy project.
