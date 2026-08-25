# Scalaxy Chat

A professional multi-room, multi-user chat built with **Phoenix
LiveView**, the **Ash Framework**, and **Scalaxy** — every room, user and
message is persisted in Scalaxy as graph nodes, written and read as
Cypher through the official `escalaxy` Elixir SDK.

There is no SQL database anywhere in this application.

## Architecture

```
Browser ──LiveView──► Phoenix ──Ash──► Chat.ScalaxyDataLayer
                                        │  (official escalaxy SDK)
                                        ▼
                                   Scalaxy node(s)
                                   (Cypher over HTTP)
                                        │
                                        ▼
                                   S3 object storage
```

* `lib/chat/scalaxy_data_layer.ex` — custom Ash data layer:
  * `create` → `CREATE (:Label {props})`
  * reads → `MATCH (n:Label) WHERE ... RETURN n`, translated to Ash structs
  * equality filters are pushed into Cypher WHERE clauses
  * UUIDv7 ids keep Cypher string sorts chronological
* `lib/chat/domain.ex` — Ash domain with `Room`, `Message`, `User` resources
* `lib/chat_web/live/room_list_live.ex` — lobby: create rooms, pick identity
* `lib/chat_web/live/chat_room_live.ex` — room: message stream, composer,
  Phoenix Presence online list; messages persist first, then fan out over PubSub

## Running

1. Start a Scalaxy cluster (see the repository's compose files), then:

```bash
cd sdks/examples/chat
mix setup
SCALAXY_URL=http://localhost:8080 SCALAXY_DB=chat mix phx.server
```

2. Open http://localhost:4000, pick a handle, create a room, share the
   room URL with other browsers/users and chat.

## Notes

* Reads of freshly written nodes rely on the cluster's background sync;
  the UI therefore delivers each message locally from the persisted
  record (persist-first, broadcast-second) rather than re-reading.
* See `scripts/KNOWN-ISSUES.md` in the main repository for two server-side
  issues the conformance harness surfaced while building this example.
