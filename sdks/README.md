# sdks

Official client SDKs for Scalaxy.

| Directory | What |
|---|---|
| [`escalaxy/`](escalaxy/) | Official Elixir client SDK (`escalaxy` hex package): HTTP client, Cypher helpers, telemetry, Ash data layer support |
| [`examples/chat/`](examples/chat/) | Example Phoenix web application — multi-room, multi-user chat built on the Ash Framework, storing all state in Scalaxy via Cypher |

Both projects are standard Mix projects:

```bash
cd escalaxy && mix test
cd examples/chat && mix phx.server
```
