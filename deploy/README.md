# Deploying Scalaxy

Scalaxy is distributed database that ships with a web console. Everything is
configured through `SCALAXY_*` environment variables, so the same image runs
in Docker, docker-compose, and Kubernetes.

## Configuration

| Variable                | Default                | Description                                  |
|-------------------------|------------------------|----------------------------------------------|
| `SCALAXY_NODE_ID`       | auto                   | Node identity (used as the ring member id)   |
| `SCALAXY_ADDRESS`       | `0.0.0.0:7200`         | Data-plane TCP listen address                |
| `SCALAXY_HTTP_ADDRESS`  | `0.0.0.0:8080`         | Web console / REST API / health listen addr  |
| `SCALAXY_DATA_DIR`      | `./scalaxy-data`       | Directory for the append-only log            |
| `SCALAXY_PEERS`         | (none)                 | Cluster topology: `id=host:data-port[:http-port],...` |
| `SCALAXY_REPLICATE_TO`  | (none)                 | Synchronous replication targets (same format)|
| `SCALAXY_WEB_DIR`       | `web/`                 | Location of the console static assets        |

When `SCALAXY_PEERS` is set, the web console routes every key operation to
the ring owner over the TCP protocol and aggregates cluster status over the
peer HTTP endpoints, so any node exposes the full cluster UI. If a peer's
HTTP port differs from the local `SCALAXY_HTTP_ADDRESS` port, append it to
that peer's entry (e.g. `node-1=10.0.0.5:7200:8080`); otherwise the local
HTTP port is assumed for all peers. Reads fail over to replica holders when
a ring owner is unreachable.

## Docker

```sh
docker build -t scalaxy .
docker run --rm -p 8080:8080 -p 7200:7200 -v scalaxy-data:/var/lib/scalaxy scalaxy
# console: http://localhost:8080
```

## docker-compose (3-node cluster)

```sh
docker compose up --build
# console: http://localhost:8080
```

## Kubernetes

```sh
kubectl apply -f deploy/kubernetes/scalaxy.yaml
kubectl -n scalaxy get pods
kubectl -n scalaxy port-forward svc/scalaxy 8080:80   # console at :8080
```

The manifest deploys a 3-replica `StatefulSet` (one PVC per node), a
headless service for peer discovery (`scalaxy-0.scalaxy-headless`), a
ClusterIP service for clients, readiness/liveness probes on `/healthz`,
and runs as non-root (uid 1000). An optional Ingress is in
`deploy/kubernetes/ingress.yaml`.

To scale, update `SCALAXY_PEERS`/`SCALAXY_REPLICATE_TO` in the ConfigMap to
include the new pods, then `kubectl scale statefulset scalaxy --replicas=5`.

## Health checks

`GET /healthz` returns `{"status":"ok"}` with HTTP 200 when the node is up
and its HTTP server is accepting connections. It is wired to the container
`HEALTHCHECK` and to the Kubernetes probes.
