# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.6.7] - 2026-08-07

First open-source release.

### Added

- Distributed key/value core in Common Lisp (SBCL):
  - consistent-hash sharding with virtual nodes (FNV-1a 64-bit + SplitMix64
    finalizer), so adding/removing a node moves only ~1/N of the keys,
  - durable append-only storage log (same record format as the network
    protocol) with exact replay on startup,
  - synchronous leader replication with per-write acks and read failover
    to replica holders,
  - key re-homing on node removal (no data loss).
- Web console written in Common Lisp with zero external dependencies:
  - dashboard (overview, data browser, cluster view, command console),
  - JSON REST API and `/healthz` endpoint,
  - dependency-free JSON encoder/decoder and HTTP/1.1 server/client.
- Cluster gateway: ring routing over TCP, per-peer status aggregation, and
  node-failure failover.
- Deployment:
  - multi-stage Dockerfile (runs the full test suite at build time),
  - docker-compose three-node cluster,
  - Kubernetes manifests (StatefulSet, PVCs, probes, headless service),
  - environment-variable configuration (`SCALAXY_*`).
- Hostname resolution for container/Kubernetes peer discovery; stable node
  identity and log filename across restarts.
- Test suite: 8,654 checks across 13 groups (storage, protocol,
  replication, clustering, TCP, HTTP, JSON, gateway routing, failover,
  hostname resolution).

### Fixed

- Consistent-hash ring distribution bias (SplitMix64 finalizer).
- Replication of writes received over the TCP data plane.
- Data loss on node removal (key re-homing) and on restart (stable
  `scalaxy.log` filename).
- Web console: Enter now runs console commands; stale UI assets are
  cache-busted.

### Security

- See [SECURITY.md](SECURITY.md) for the reporting process and design notes.
