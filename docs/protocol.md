

## Framing

Every message is a **length-prefixed frame**:

```
[4-byte big-endian body length][body]
body := [1-byte opcode][opcode-specific payload]
```

The same record format is used for:

- client requests over the data-plane TCP port (7200),
- replication messages between nodes,
- the append-only durability log (so log records can be replayed and replicated verbatim).
Cypher queries run over the same frames: a `CYPHER` request carries the
database, the query text, and JSON parameters, and the reply is a
`RESPONSE` whose value is the JSON result table.

## Opcodes

| Opcode | Value | Payload |
|--------|------:|---------|
| `PUT` | 1 | key (length-prefixed string) + value (length-prefixed octets) |
| `GET` | 2 | key |
| `DELETE` | 3 | key |
| `SCAN` | 4 | prefix |
| `REPLICATE` | 5 | seq (u64) + sub-op (u8) + put/delete payload |
| `ACK` | 6 | seq (u64, 0 if unused) + status (u8) |
| `ERROR` | 7 | message string |
| `PING` | 8 | — |
| `PONG` | 9 | — |
| `SNAPSHOT` | 10 | count (u32) + (key, value) pairs |
| `RESPONSE` | 11 | status (u8) + value octets + count (u32) + pairs |
| `CYPHER` | 12 | db (string) + query (string) + params (JSON octets) |

## Primitives

- **u8 / u32 / u64** — unsigned big-endian integers.
- **length-prefixed string** — u32 length + UTF-8/ASCII bytes.
- **octets** — u32 length + raw bytes.
- **status** — `0` = OK, `1` = not found.

## Example: GET round trip

```text
client -> node:  00 00 00 05 02 00 00 00 03 6b 65 79     (GET "key")
node   -> client: 00 00 00 0d 0b 00 00 00 00 00 00 00 03 76 61 6c
                                                     (RESPONSE status=0 value="val")
```

## Implementation

The encoder/decoder live in `src/protocol.lisp` (`encode-message`, `decode-message`, `frame-message`). The TCP transport is in `src/tcp.lisp`; hostnames are resolved with `resolve-host`, so peers can be addressed by DNS name in containers and Kubernetes.

> **Note:** 
The protocol is small on purpose: it keeps the database easy to audit, reimplement, and embed. A full reference of every opcode's exact byte layout is in the docstrings of `src/protocol.lisp`.

