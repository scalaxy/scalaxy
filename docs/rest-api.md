

The web console is served by each node over HTTP (default port 8080). With `SCALAXY_PEERS` set, the API is **cluster-aware**: key operations route to ring owners, and status is aggregated across all peers.

## Endpoints

### Health

```text
GET /healthz
```

Returns `{"status":"ok"}` with HTTP 200. Used by the container `HEALTHCHECK` and the Kubernetes readiness/liveness probes.

### Cluster status

```text
GET /api/status
```

```json
{
  "node":   {"id":"node-0","address":"0.0.0.0:7200","http":"0.0.0.0:8080",
             "keys":18,"uptime":42,"replicas":1,"version":"1.6.7","status":"ok"},
  "nodes":  [{"id":"node-1","keys":27,"status":"ok"},
             {"id":"node-2","keys":15,"status":"ok"}],
  "cluster":{"nodes":3,"keys":60,"replicas":1,"status":"healthy"},
  "ring":   [{"id":"node-0","share":0.33},{"id":"node-1","share":0.33}]
}
```

### Key list

```text
GET /api/keys?prefix=cust:&limit=40&offset=0
```

```json
{"keys":[{"key":"cust:7","size":2,"preview":"v7"}],"total":30,"limit":40,"offset":0}
```

### Key read / write / delete

```text
GET    /api/keys/<key>     # -> {"key":"cust:7","size":2,"utf8":"v7","hex":"7637"}
PUT    /api/keys/<key>     # body {"value":"..."} -> {"ok":true,"key":"cust:7","size":2}
DELETE /api/keys/<key>     # -> {"ok":true}
```

Values may be any UTF-8 string; binary values are visible via the `hex` field.

### Command console

```text
POST /api/query
{"command":"scan cust:"}
```

```json
{"ok":true,"output":"2 keys matching \"cust:\":\ncust:1  (2 bytes)\ncust:2  (2 bytes)"}
```

Supported commands: `put <key> <value>`, `get <key>`, `delete <key>`, `scan <prefix> [limit]`.

### Node-local status

```text
GET /api/node-status
```

The per-node endpoint used by gateway aggregation: id, address, http, keys, uptime, replicas, version, status.

### Console assets

```text
GET /            # dashboard (index.html)
GET /assets/app.css
GET /assets/app.js
```

## Error handling

Errors return JSON with an `error` field and the appropriate status code:

```json
{"error":"not found"}
```

`404` for unknown routes or missing keys, `400` for malformed bodies, `500` for internal errors.
