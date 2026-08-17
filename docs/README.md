# Documentation

The full documentation portal lives at <https://scalaxy.org/docs/> (developer
and maintainer guides: getting started, architecture, protocol, REST API,
client API, deployment, Kubernetes, operations, contributing, releasing).

This directory mirrors the core reference documentation so it is available
in-repository:

- [protocol.md](protocol.md) — the binary wire format shared by the data
  plane and the durability log (including the `CYPHER` opcode).
- [rest-api.md](rest-api.md) — the HTTP API behind the web console
  (including `POST /api/cypher`).
- [cypher-implementation-plan.md](cypher-implementation-plan.md) — the
  axiomatic design for the graph layer and the openCypher engine.
- [cypher-reference.md](cypher-reference.md) — the openCypher language
  reference implemented by Scalaxy.
- [cypher-certification.md](cypher-certification.md) — the conformance
  report against the openCypher TCK.

## Quick pointers

| Topic | Where |
|---|---|
| Getting started | `README.md` and <https://scalaxy.org/docs/getting-started/> |
| Architecture | <https://scalaxy.org/docs/architecture/> |
| Wire protocol | `docs/protocol.md` |
| REST API | `docs/rest-api.md` |
| Deployment (Docker/compose/K8s) | <https://scalaxy.org/docs/deployment/> |
| Contributing | `CONTRIBUTING.md` |
| Security | `SECURITY.md` |
