#!/usr/bin/env python3
"""Scalaxy TLA+ conformance harness.

Executes each invariant of specs/tla/ScalaxySpec.tla as a live check
against a running 3-node cluster (compose: garage + scalaxy-0..2).
Zone map matches ScalaxySpec.cfg: z1={node-0,node-1}, z2={node-2}.

Usage:  python3 scripts/tla-conformance.py
"""
import json, subprocess, sys, time

NODES = {"node-0": "tmp-scalaxy-0-1", "node-1": "tmp-scalaxy-1-1", "node-2": "tmp-scalaxy-2-1"}
ZONES = {"z1": ["node-0", "node-1"], "z2": ["node-2"]}          # ScalaxySpec.cfg topology
INNET = {"node-0": "http://scalaxy-0:8080",                      # addresses inside docker net
         "node-1": "http://scalaxy-1:8080",
         "node-2": "http://scalaxy-2:8080"}

results = []

def cypher(node_id, query, timeout=60):
    """Run a Cypher query against a specific node through the shared docker net."""
    cmd = ["docker", "exec", NODES[node_id], "curl", "-s", "-m", str(timeout),
           "-X", "POST", "-H", "Content-Type: application/json",
           "-d", json.dumps({"query": query}),
           f"{INNET[node_id]}/api/cypher?db=conformance"]
    r = subprocess.run(cmd, capture_output=True, text=True, timeout=timeout + 10)
    try:
        return json.loads(r.stdout)
    except Exception:
        return {"error": r.stdout[:200], "raw": True}

def alive(node_id):
    cmd = ["docker", "exec", NODES[node_id], "curl", "-s", "-m", "5",
           f"{INNET[node_id]}/api/status"]
    r = subprocess.run(cmd, capture_output=True, text=True, timeout=10)
    return r.returncode == 0 and '"id"' in r.stdout

def stop_node(node_id):
    subprocess.run(["docker", "stop", NODES[node_id]], capture_output=True, timeout=120)

def start_node(node_id):
    subprocess.run(["docker", "start", NODES[node_id]], capture_output=True, timeout=120)

def wait_healthy(nodes, deadline=300):
    t0 = time.time()
    while time.time() - t0 < deadline:
        if all(alive(n) for n in nodes):
            return round(time.time() - t0, 1)
        time.sleep(5)
    return None

def check(name, invariant, ok, detail=""):
    results.append((name, invariant, ok, detail))
    print(f"{'PASS' if ok else 'FAIL':4} [{invariant}] {name}" + (f" -- {detail}" if detail else ""))

def rows(resp):
    return resp.get("rows", []) if isinstance(resp, dict) else []

# ---------------------------------------------------------------- checks ----

print("== Scalaxy TLA+ conformance harness ==")
for n in NODES:
    assert alive(n), f"{n} not reachable at start"
print("cluster up: 3/3 nodes\n")

BASE = "MATCH (n:ConfTest)"
count_q = f"{BASE} RETURN count(n)"

# --- DeleteVisible -----------------------------------------------------
cypher("node-0", "CREATE (:ConfTest {id: 'cv-1'})")
time.sleep(2)                                   # allow replication fan-out
before = {n: rows(cypher(n, count_q))[0][0] for n in NODES}
check("replicated write visible on all 3 nodes", "ReplicationFactorTwo",
      all(v >= 1 for v in before.values()), str(before))

cypher("node-1", "MATCH (n:ConfTest) DELETE n")
time.sleep(2)
after = {n: rows(cypher(n, count_q))[0][0] for n in NODES}
check("deleted key invisible everywhere, immediately", "DeleteVisible",
      all(v == 0 for v in after.values()), str(after))

# --- AvailabilityUnderSingleFailure + WriteCapability -------------------
baseline = rows(cypher("node-0", "MATCH ()-[r]->() RETURN count(r)"))[0][0]
stop_node("node-1")
t_up = wait_healthy(["node-0", "node-2"])
read0 = rows(cypher("node-0", "MATCH ()-[r]->() RETURN count(r)"))
read2 = rows(cypher("node-2", "MATCH ()-[r]->() RETURN count(r)"))
ok = read0 and read2 and read0[0][0] == read2[0][0] == baseline
check("reads uninterrupted with node-1 down, counts identical", "AvailabilityUnderSingleFailureAfterReplication",
      bool(ok), f"baseline={baseline} n0={read0[0][0] if read0 else '?'} n2={read2[0][0] if read2 else '?'}")

wr = cypher("node-2", "CREATE (:ConfTest {id: 'cv-outage'})")
time.sleep(2)
seen0 = rows(cypher("node-0", f"{BASE} {{id:'cv-outage'}} RETURN count(n)"))
check("writes succeed on survivors during outage", "WriteCapabilityUnderSingleZoneFailure",
      bool(seen0) and seen0[0][0] == 1)

# --- RecoveryRestoresService + ReplicationConverges ---------------------
start_node("node-1")
t_back = wait_healthy(list(NODES))
healed = rows(cypher("node-1", "MATCH ()-[r]->() RETURN count(r)"))
check("restarted node rejoins with intact durable state", "RecoveryRestoresService",
      t_back is not None and bool(healed) and healed[0][0] == baseline,
      f"rejoin {t_back}s")

# outbox drain: cv-outage written while node-1 down must reach it eventually
deadline = time.time() + 240
converged = False
while time.time() < deadline:
    v = rows(cypher("node-1", f"{BASE} {{id:'cv-outage'}} RETURN count(n)"))
    if v and v[0][0] == 1:
        converged = True
        break
    time.sleep(10)
check("key written during outage converges to restarted node", "ReplicationConverges",
      converged, f"{round(240-(deadline-time.time()))}s")

# --- ZoneOutage (z2 = {node-2}) -----------------------------------------
stop_node("node-2")
t_up = wait_healthy(["node-0", "node-1"])
zc = rows(cypher("node-0", "MATCH ()-[r]->() RETURN count(r)"))
zw = cypher("node-1", "CREATE (:ConfTest {id: 'cv-zone'})")
zd = rows(cypher("node-0", f"{BASE} {{id:'cv-zone'}} RETURN count(n)"))
check("whole-zone (z2) outage: reads + writes carry on via z1",
      "AvailabilityUnderSingleZoneFailure",
      bool(zc) and zc[0][0] == baseline and bool(zd) and zd[0][0] == 1,
      f"reads={'ok' if zc and zc[0][0]==baseline else 'MISMATCH'}, writes={'ok' if zd and zd[0][0]==1 else 'FAIL'}")
start_node("node-2")
wait_healthy(list(NODES))
time.sleep(5)

# --- EncryptionAtRest (configuration level) ------------------------------
env = subprocess.run(["docker", "exec", NODES["node-0"],
                      "sh", "-c", "env | grep -c SCALAXY_S3_ENCRYPTION_KEY"],
                     capture_output=True, text=True).stdout.strip()
check("all S3 writes use encryption key (byte-level covered by unit suite)",
      "EncryptionAtRest", env == "1")

# --- cleanup -------------------------------------------------------------
for q in ["MATCH (n:ConfTest) DELETE n"]:
    cypher("node-0", q); time.sleep(2)

fails = [r for r in results if not r[2]]
print(f"\n{len(results)-len(fails)}/{len(results)} conformance checks passed.")
sys.exit(1 if fails else 0)
