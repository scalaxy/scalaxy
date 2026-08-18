/* Scalaxy console */
"use strict";

const $ = (id) => document.getElementById(id);

const state = {
  page: "overview",
  prefix: "",
  limit: 40,
  offset: 0,
  total: 0,
  status: null,
};

/* current database (per browser, per cluster) */
function currentDb() { return localStorage.getItem("scalaxy-db") || "default"; }
function setDb(name) {
  localStorage.setItem("scalaxy-db", name);
  const hint = document.getElementById("console-hint");
  if (hint) hint.textContent = "Enter \u23ce to run \u00b7 Shift+Enter for newline \u00b7 db: " + name;
}

const PALETTE = ["#4f8cff", "#3fb950", "#d29922", "#f85149", "#a371f7",
                 "#39c5cf", "#ffa657", "#f778ba", "#7ee787", "#e3b341"];

/* ---------- helpers ---------- */
async function api(method, path, body) {
  const opts = { method, headers: {} };
  if (body !== undefined) {
    opts.headers["Content-Type"] = "application/json";
    opts.body = JSON.stringify(body);
  }
  const res = await fetch(path, opts);
  let data = null;
  try { data = await res.json(); } catch (e) { /* non-JSON */ }
  if (!res.ok) throw new Error((data && data.error) || ("HTTP " + res.status));
  return data;
}

function esc(s) {
  const d = document.createElement("div");
  d.textContent = s == null ? "" : String(s);
  return d.innerHTML;
}

function fmtUptime(secs) {
  if (secs == null) return "-";
  const d = Math.floor(secs / 86400), h = Math.floor((secs % 86400) / 3600),
        m = Math.floor((secs % 3600) / 60);
  if (d > 0) return d + "d " + h + "h";
  if (h > 0) return h + "h " + m + "m";
  return m + "m";
}

function fmtBytes(n) {
  if (n == null) return "-";
  if (n < 1024) return n + " B";
  if (n < 1048576) return (n / 1024).toFixed(1) + " KiB";
  return (n / 1048576).toFixed(1) + " MiB";
}

/* ---------- navigation ---------- */
function showPage(name) {
  state.page = name;
  document.querySelectorAll(".nav-item").forEach((a) =>
    a.classList.toggle("active", a.dataset.page === name));
  document.querySelectorAll(".page").forEach((s) =>
    s.classList.toggle("hidden", s.id !== "page-" + name));
  const titles = { overview: "Overview", data: "Data", cluster: "Cluster", console: "Console", graph: "Graph" };
  $("page-title").textContent = titles[name];
  if (name === "data") loadKeys();
  else if (name === "console") setTimeout(() => $("console-input").focus(), 50);
  else if (name === "graph") { if (!graphState.query) loadGraphSample(); loadStatus(); }
  else loadStatus();
}

document.querySelectorAll(".nav-item").forEach((a) =>
  a.addEventListener("click", () => showPage(a.dataset.page)));
$("btn-refresh").addEventListener("click", () => {
  loadStatus();
  if (state.page === "data") loadKeys();
});

/* ---------- status / overview ---------- */
async function loadStatus() {
  let data;
  try { data = await api("GET", "/api/status"); }
  catch (e) {
    setPill("down", "unreachable");
    $("stat-status").textContent = "down";
    return;
  }
  state.status = data;

  const cluster = data.cluster || {};
  const status = cluster.status || "unknown";
  setPill(status === "healthy" ? "ok" : "degraded", status);
  $("stat-nodes").textContent = cluster.nodes != null ? cluster.nodes : "-";
  $("stat-keys").textContent = cluster.keys != null ? cluster.keys : "-";
  $("stat-replicas").textContent = cluster.replicas != null ? cluster.replicas : "-";
  $("stat-status").textContent = status;
  $("stat-nodes-sub").textContent = (data.ring || []).map((r) => r.id).join(", ");

  const node = data.node || {};
  $("foot-node").textContent = node.id || "-";
  $("foot-version").textContent = node.version || "-";
  $("foot-uptime").textContent = fmtUptime(node.uptime);
  const ver = $("brand-version");
  if (ver) ver.textContent = (node.version ? "v" + node.version : "v?");
  if (document.title && node.version) {
    document.title = document.title.replace(/\bv\d+\.\d+\.\d+\b/, "v" + node.version);
  }

  const nodes = data.nodes || [];
  const ring = data.ring || [];
  renderNodes(nodes, status);
  renderRing(ring, cluster.keys || 0);
  renderClusterNodes(nodes);
  renderRingBars(ring);
}

function setPill(kind, text) {
  const pill = $("cluster-pill");
  pill.innerHTML = '<span class="dot ' + kind + '"></span>' + esc(text);
}

function renderRing(ring, totalKeys) {
  const svg = $("ring-svg");
  svg.innerHTML = "";
  const NS = "http://www.w3.org/2000/svg";
  const cx = 100, cy = 100, r = 82, C = 2 * Math.PI * r;
  let offset = 0;
  ring.forEach((node, i) => {
    const len = Math.max(0.0001, node.share * C);
    const circ = document.createElementNS(NS, "circle");
    circ.setAttribute("cx", cx); circ.setAttribute("cy", cy); circ.setAttribute("r", r);
    circ.setAttribute("fill", "none");
    circ.setAttribute("stroke", PALETTE[i % PALETTE.length]);
    circ.setAttribute("stroke-width", "26");
    circ.setAttribute("stroke-dasharray", len.toFixed(2) + " " + (C - len).toFixed(2));
    circ.setAttribute("stroke-dashoffset", (-offset).toFixed(2));
    circ.setAttribute("transform", "rotate(-90 " + cx + " " + cy + ")");
    circ.setAttribute("opacity", "0.9");
    svg.appendChild(circ);
    offset += len;
  });
  $("ring-total").textContent = totalKeys != null ? totalKeys : "-";
}

function renderNodes(nodes, clusterStatus) {
  const body = $("overview-nodes");
  if (!nodes.length) { body.innerHTML = '<tr><td colspan="4" class="empty">no nodes</td></tr>'; return; }
  body.innerHTML = "";
  nodes.forEach((n) => {
    const st = (n.status || "unknown") === "ok" ? "ok" : "degraded";
    const tr = document.createElement("tr");
    tr.innerHTML =
      '<td class="key-cell">' + esc(n.id) + "</td>" +
      '<td class="num">' + (n.keys != null ? n.keys : "-") + "</td>" +
      '<td>' + fmtUptime(n.uptime) + "</td>" +
      '<td><span class="badge ' + st + '">' + st + "</span></td>";
    body.appendChild(tr);
  });
}

/* ---------- data ---------- */
async function loadKeys() {
  const prefix = $("key-prefix").value;
  const q = new URLSearchParams({ prefix, limit: state.limit, offset: state.offset, db: currentDb() });
  let data;
  try { data = await api("GET", "/api/keys?" + q.toString()); }
  catch (e) { $("keys-body").innerHTML = '<tr><td colspan="4" class="empty">' + esc(e.message) + "</td></tr>"; return; }
  state.total = data.total || 0;
  renderKeys(data.keys || []);
  $("keys-info").textContent =
    state.total === 0 ? "no keys" :
    "showing " + (state.offset + 1) + "\u2013" + Math.min(state.offset + state.limit, state.total) +
    " of " + state.total;
  $("btn-prev").disabled = state.offset <= 0;
  $("btn-next").disabled = state.offset + state.limit >= state.total;
}

function renderKeys(keys) {
  const body = $("keys-body");
  if (!keys.length) { body.innerHTML = '<tr><td colspan="4" class="empty">no keys match</td></tr>'; return; }
  body.innerHTML = "";
  keys.forEach((k) => {
    const tr = document.createElement("tr");
    tr.innerHTML =
      '<td class="key-cell">' + esc(k.key) + "</td>" +
      '<td class="num">' + fmtBytes(k.size) + "</td>" +
      '<td class="preview-cell">' + esc(k.preview || "") + "</td>" +
      '<td class="th-actions">' +
        '<button class="btn btn-ghost" data-view="' + esc(k.key) + '">view</button> ' +
        '<button class="btn btn-ghost" data-del="' + esc(k.key) + '">del</button>' +
      "</td>";
    body.appendChild(tr);
  });
  body.querySelectorAll("[data-view]").forEach((b) =>
    b.addEventListener("click", () => viewKey(b.dataset.view)));
  body.querySelectorAll("[data-del]").forEach((b) =>
    b.addEventListener("click", () => deleteKey(b.dataset.del)));
}

$("btn-search").addEventListener("click", () => { state.offset = 0; loadKeys(); });
$("key-prefix").addEventListener("keydown", (e) => { if (e.key === "Enter") { state.offset = 0; loadKeys(); } });
$("btn-prev").addEventListener("click", () => { state.offset = Math.max(0, state.offset - state.limit); loadKeys(); });
$("btn-next").addEventListener("click", () => { state.offset += state.limit; loadKeys(); });

/* ---------- key view / add / delete ---------- */
async function viewKey(key) {
  try {
    const d = await api("GET", "/api/keys/" + encodeURIComponent(key) + "?db=" + encodeURIComponent(currentDb()));
    $("modal-title").textContent = key;
    $("mv-size").textContent = fmtBytes(d.size);
    $("mv-utf8").textContent = d.utf8;
    $("mv-hex").textContent = d.hex;
    $("modal-overlay").classList.remove("hidden");
    $("mv-delete").onclick = async () => { await deleteKey(key); closeModal(); };
  } catch (e) { alert(e.message); }
}

function closeModal() {
  $("modal-overlay").classList.add("hidden");
  $("add-overlay").classList.add("hidden");
}

async function deleteKey(key) {
  if (!confirm('Delete key "' + key + '"?')) return;
  try { await api("DELETE", "/api/keys/" + encodeURIComponent(key) + "?db=" + encodeURIComponent(currentDb())); loadKeys(); }
  catch (e) { alert(e.message); }
}

$("btn-add").addEventListener("click", () => $("add-overlay").classList.remove("hidden"));
$("add-close").addEventListener("click", closeModal);
$("modal-close").addEventListener("click", closeModal);
$("modal-overlay").addEventListener("click", (e) => { if (e.target === $("modal-overlay")) closeModal(); });
$("add-overlay").addEventListener("click", (e) => { if (e.target === $("add-overlay")) closeModal(); });

$("add-save").addEventListener("click", async () => {
  const key = $("add-key").value.trim();
  const value = $("add-value").value;
  if (!key) { alert("key required"); return; }
  try {
    await api("PUT", "/api/keys/" + encodeURIComponent(key) + "?db=" + encodeURIComponent(currentDb()), { value });
    $("add-key").value = ""; $("add-value").value = "";
    closeModal();
    loadKeys();
  } catch (e) { alert(e.message); }
});

/* ---------- console ---------- */
const CONSOLE_HELP =
  "usage: put <key> <value> | get <key> | delete <key> | scan <prefix> [limit]\n" +
  "db: use <name> | databases   (current: " + currentDb() + ")\n" +
  "example: scan user:" +
  "\n\nEnter runs the command, Shift+Enter inserts a newline.";

function bindRun() {
  const btn = document.getElementById("btn-run");
  const input = document.getElementById("console-input");
  if (btn && !btn.dataset.bound) {
    btn.dataset.bound = "1";
    btn.addEventListener("click", (e) => {
      e.preventDefault();
      runQuery();
    });
  }
  if (input && !input.dataset.bound) {
    input.dataset.bound = "1";
    input.addEventListener("keydown", (e) => {
      // Enter runs the command; Shift+Enter inserts a newline
      if (e.key === "Enter" && !e.shiftKey && !e.isComposing) {
        e.preventDefault();
        runQuery();
      }
    });
  }
}
// direct binding + delegation fallback so the button always works
bindRun();
document.addEventListener("click", (e) => {
  const t = e.target && e.target.closest ? e.target.closest("#btn-run") : null;
  if (t) runQuery();
});

async function runQuery() {
  const input = document.getElementById("console-input");
  const btn = document.getElementById("btn-run");
  const out = document.getElementById("console-output");
  if (!input || !out) return;
  const command = input.value.trim();
  if (!command) {
    out.textContent = CONSOLE_HELP;
    out.scrollTop = out.scrollHeight;
    if (input) input.focus();
    return;
  }
  if (btn) btn.disabled = true;
  out.textContent = "running: " + command + "\n";
  try {
    const d = await api("POST", "/api/query", { command, db: currentDb() });
    if (d.ok && /^use\s+\S+/.test(command)) {
      const name = (d.output || "").trim();
      setDb(name);
      out.textContent += "switched to database: " + name;
    } else {
      out.textContent += (d.ok ? "" : "error: ") + (d.output || "");
    }
    out.scrollTop = out.scrollHeight;
    if (state.page === "data") loadKeys();
    if (state.page === "overview") loadStatus();
  } catch (e) {
    out.textContent += "error: " + e.message;
  } finally {
    if (btn) btn.disabled = false;
    if (input) input.focus();
  }
}

/* ---------- cluster page ---------- */
function renderClusterNodes(nodes) {
  const body = $("cluster-nodes");
  if (!nodes.length) { body.innerHTML = '<tr><td colspan="6" class="empty">no nodes</td></tr>'; return; }
  body.innerHTML = "";
  nodes.forEach((n) => {
    const st = n.status || "unknown";
    const cls = st === "ok" ? "ok" : "down";
    const tr = document.createElement("tr");
    tr.innerHTML =
      '<td class="key-cell">' + esc(n.id) + "</td>" +
      '<td class="mono">' + esc(n.address || "-") + "</td>" +
      '<td class="mono">' + esc(n.http || "-") + "</td>" +
      '<td class="num">' + (n.keys != null ? n.keys : "-") + "</td>" +
      '<td class="num">' + (n.replicas != null ? n.replicas : "-") + "</td>" +
      '<td><span class="badge ' + cls + '">' + esc(st) + "</span></td>";
    body.appendChild(tr);
  });
}

function renderRingBars(ring) {
  const wrap = $("ring-bars");
  wrap.innerHTML = "";
  if (!ring.length) { wrap.innerHTML = '<div class="empty">no nodes</div>'; return; }
  ring.forEach((node, i) => {
    const pct = Math.round((node.share || 0) * 100);
    const row = document.createElement("div");
    row.className = "bar-row";
    row.innerHTML =
      '<div class="bar-label"><span>' + esc(node.id) + "</span><span>" + pct + "%</span></div>" +
      '<div class="bar-track"><div class="bar-fill" style="width:' + pct + '%;background:' +
      PALETTE[i % PALETTE.length] + '"></div></div>';
    wrap.appendChild(row);
  });
}

/* ---------- init ---------- */
showPage("overview");
setInterval(() => { if (state.page === "overview" || state.page === "cluster") loadStatus(); }, 5000);

/* ================= GraphQL + interactive 2D graph ================= */

const graphState = {
  query: "",
  nodes: [],          // {id, labels, properties}
  edges: [],          // {id, type, start, end, properties}
  running: false,
  view: "graph",      // "graph" | "json"
  /* force layout state */
  pos: new Map(),     // id -> {x, y, vx, vy, r}
  cam: { x: 0, y: 0, z: 1 },   // camera offset + scale
  drag: null,         // {id, dx, dy} while dragging a node
  pan: null,          // {sx, sy, cx, cy} while panning
};

const GQL_SAMPLE = `# Pull every node and edge in the graph.
# Nested selections become the visualization; the server also returns
# extensions.graph with exactly the materialized nodes and edges.
{
  nodes(limit: 200) {
    id
    labels
    properties
    relationships(direction: BOTH, limit: 200) {
      id
      type
      properties
      to { id labels properties }
    }
  }
}`;

function loadGraphSample() {
  const el = $("gql-input");
  if (el && !el.value.trim()) {
    el.value = GQL_SAMPLE;
    graphState.query = GQL_SAMPLE;
  }
}

function setGqlStats(text) { const el = $("gql-stats"); if (el) el.textContent = text; }

function labelOf(n) {
  return (n.labels && n.labels.length) ? n.labels[0] : "?";
}

function nodeTitle(n) {
  const p = n.properties || {};
  const t = p.name || p.title || p.label || p.id;
  return String(t);
}

function edgeLabel(e) { return e.type || "?"; }

/* ---------- query execution ---------- */
async function runGraphql() {
  const input = $("gql-input");
  if (!input) return;
  const query = input.value.trim();
  if (!query) return;
  if (graphState.running) return;
  graphState.running = true;
  const btn = $("gql-run");
  if (btn) btn.disabled = true;
  setGqlStats("running\u2026");
  try {
    const t0 = performance.now();
    const d = await api("POST", "/api/graphql", { query, db: currentDb() });
    const ms = Math.round(performance.now() - t0);
    graphState.query = query;
    if (d.errors && d.errors.length) {
      const msg = d.errors.map((e) => e.message || "error").join("\n");
      setGqlStats("error");
      $("gql-json").textContent = msg;
      $("gql-json").classList.remove("hidden");
      $("gql-graph-wrap").classList.add("hidden");
      graphState.view = "json";
      setSegActive("json");
      return;
    }
    /* collect nodes + edges from extensions.graph (fallback: walk data) */
    const ext = d.extensions && d.extensions.graph;
    let nodes = [], edges = [];
    if (ext && ext.nodes) {
      nodes = ext.nodes;
      edges = ext.edges || [];
    } else {
      const collected = collectGraphFromData(d.data);
      nodes = collected.nodes; edges = collected.edges;
    }
    graphState.nodes = nodes;
    graphState.edges = edges;
    setGqlStats(nodes.length + " nodes \u00b7 " + edges.length + " edges \u00b7 " + ms + " ms");
    $("gql-json").textContent = JSON.stringify(d.data, null, 2);
    showGraphView();
    renderGraph();
  } catch (e) {
    setGqlStats("error");
    $("gql-json").textContent = "error: " + e.message;
    $("gql-json").classList.remove("hidden");
    $("gql-graph-wrap").classList.add("hidden");
    graphState.view = "json";
    setSegActive("json");
  } finally {
    graphState.running = false;
    if (btn) btn.disabled = false;
  }
}

/* fallback: walk the data tree, recognizing node/edge shapes */
function collectGraphFromData(v, nodes, edges, seenN, seenE) {
  nodes = nodes || []; edges = edges || [];
  seenN = seenN || new Set(); seenE = seenE || new Set();
  const walk = (x) => {
    if (Array.isArray(x)) { x.forEach(walk); return; }
    if (x && typeof x === "object") {
      const id = x.id, labels = x.labels;
      if (typeof id === "string" && Array.isArray(labels) && !seenN.has(id)) {
        seenN.add(id);
        nodes.push({ id, labels: labels, properties: x.properties || {} });
      }
      const type = x.type;
      if (typeof id === "string" && typeof type === "string" &&
          (x.to || x.from || x.start || x.end) && !seenE.has(id)) {
        seenE.add(id);
        edges.push({ id, type, start: (x.from || x.start || {}).id || x.start,
                     end: (x.to || x.end || {}).id || x.end,
                     properties: x.properties || {} });
      }
      Object.keys(x).forEach((k) => { if (k !== "properties") walk(x[k]); });
    }
  };
  walk(v);
  return { nodes, edges };
}

function setSegActive(which) {
  graphState.view = which;
  $("seg-graph").classList.toggle("active", which === "graph");
  $("seg-json").classList.toggle("active", which === "json");
}

function showGraphView() {
  $("gql-json").classList.toggle("hidden", graphState.view !== "json");
  $("gql-graph-wrap").classList.toggle("hidden", graphState.view !== "graph");
  if (graphState.view === "graph") {
    // give the canvas a frame to size before drawing
    setTimeout(() => drawGraph(), 30);
  }
}

/* ---------- force-directed layout ---------- */
function layoutGraph() {
  const nodes = graphState.nodes, edges = graphState.edges;
  const W = Math.max(300, window.innerWidth * 0.5), H = Math.max(300, window.innerHeight * 0.6);
  const byId = new Map(nodes.map((n) => [n.id, n]));
  /* adjacency for repulsion/springs */
  const adj = new Map(nodes.map((n) => [n.id, []]));
  edges.forEach((e) => {
    if (adj.has(e.start)) adj.get(e.start).push(e.end);
    if (adj.has(e.end)) adj.get(e.end).push(e.start);
  });
  /* deterministic initial placement (ring) */
  const pos = graphState.pos;
  nodes.forEach((n, i) => {
    if (!pos.has(n.id)) {
      const ang = (i / Math.max(1, nodes.length)) * Math.PI * 2;
      const rad = Math.min(W, H) * 0.38;
      pos.set(n.id, { x: W / 2 + Math.cos(ang) * rad, y: H / 2 + Math.sin(ang) * rad, vx: 0, vy: 0, r: 14 });
    }
  });
  /* simulation */
  const C = 120, K = 90, REP = 900, dt = 0.55, steps = 260;
  for (let s = 0; s < steps; s++) {
    const cool = 1 - s / steps;
    nodes.forEach((n) => {
      const p = pos.get(n.id);
      p.vx *= 0.86; p.vy *= 0.86;
      /* repulsion from all others (Barnes-Hut omitted: corpus is small) */
      nodes.forEach((m) => {
        if (m.id === n.id) return;
        const q = pos.get(m.id);
        let dx = p.x - q.x, dy = p.y - q.y;
        let d2 = dx * dx + dy * dy;
        if (d2 < 1) { dx = (Math.random() - 0.5); dy = (Math.random() - 0.5); d2 = 1; }
        const f = REP / d2;
        const d = Math.sqrt(d2);
        p.vx += (dx / d) * f * cool;
        p.vy += (dy / d) * f * cool;
      });
      /* springs along edges */
      adj.get(n.id).forEach((other) => {
        const q = pos.get(other);
        if (!q) return;
        let dx = q.x - p.x, dy = q.y - p.y;
        const d = Math.max(1, Math.sqrt(dx * dx + dy * dy));
        const f = (d - C) * 0.04 * cool;
        p.vx += (dx / d) * f;
        p.vy += (dy / d) * f;
      });
      /* centering */
      p.vx += (W / 2 - p.x) * 0.004 * cool;
      p.vy += (H / 2 - p.y) * 0.004 * cool;
    });
    nodes.forEach((n) => {
      const p = pos.get(n.id);
      p.x += p.vx * dt; p.y += p.vy * dt;
      p.x = Math.max(30, Math.min(W - 30, p.x));
      p.y = Math.max(30, Math.min(H - 30, p.y));
    });
  }
  /* degree-based radius + label */
  nodes.forEach((n) => {
    const p = pos.get(n.id);
    const deg = adj.get(n.id).length;
    p.r = 9 + Math.min(12, Math.sqrt(deg) * 3);
  });
  /* fit camera to the layout */
  let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity;
  nodes.forEach((n) => {
    const p = pos.get(n.id);
    if (p.x < minX) minX = p.x; if (p.x > maxX) maxX = p.x;
    if (p.y < minY) minY = p.y; if (p.y > maxY) maxY = p.y;
  });
  if (nodes.length > 1 && maxX > minX) {
    const pad = 60;
    const cw = $("gql-canvas").clientWidth || 800, ch = $("gql-canvas").clientHeight || 500;
    const z = Math.min(cw / (maxX - minX + pad * 2), ch / (maxY - minY + pad * 2), 1.4);
    graphState.cam = {
      x: cw / 2 - ((minX + maxX) / 2) * z,
      y: ch / 2 - ((minY + maxY) / 2) * z,
      z: Math.max(0.15, z),
    };
  }
}

/* ---------- canvas rendering ---------- */
function renderGraph() {
  if (!graphState.nodes.length) {
    setGqlStats("0 nodes \u00b7 0 edges");
    $("gql-empty").style.display = "flex";
    drawGraph();
    return;
  }
  $("gql-empty").style.display = "none";
  layoutGraph();
  drawGraph();
}

function drawGraph() {
  const canvas = $("gql-canvas");
  if (!canvas) return;
  const dpr = window.devicePixelRatio || 1;
  const cw = canvas.clientWidth || canvas.parentElement.clientWidth || 800;
  const ch = canvas.clientHeight || canvas.parentElement.clientHeight || 500;
  canvas.width = Math.round(cw * dpr);
  canvas.height = Math.round(ch * dpr);
  const ctx = canvas.getContext("2d");
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
  ctx.clearRect(0, 0, cw, ch);
  const cam = graphState.cam;
  const toX = (x) => x * cam.z + cam.x;
  const toY = (y) => y * cam.z + cam.y;
  const colorFor = (label) => {
    let h = 0;
    for (let i = 0; i < label.length; i++) h = (h * 31 + label.charCodeAt(i)) >>> 0;
    return PALETTE[h % PALETTE.length];
  };
  /* edges */
  const byId = new Map(graphState.nodes.map((n) => [n.id, n]));
  graphState.edges.forEach((e) => {
    const a = graphState.pos.get(e.start), b = graphState.pos.get(e.end);
    if (!a || !b) return;
    const ax = toX(a.x), ay = toY(a.y), bx = toX(b.x), by = toY(b.y);
    ctx.strokeStyle = "rgba(139,148,158,0.55)";
    ctx.lineWidth = 1.2;
    ctx.beginPath();
    ctx.moveTo(ax, ay);
    ctx.lineTo(bx, by);
    ctx.stroke();
    /* arrow head */
    const ang = Math.atan2(by - ay, bx - ax);
    const r = (b.r || 12) * cam.z + 3;
    const hx = bx - Math.cos(ang) * r, hy = by - Math.sin(ang) * r;
    ctx.fillStyle = "rgba(139,148,158,0.8)";
    ctx.beginPath();
    ctx.moveTo(hx, hy);
    ctx.lineTo(hx - Math.cos(ang - 0.42) * 9 * cam.z, hy - Math.sin(ang - 0.42) * 9 * cam.z);
    ctx.lineTo(hx - Math.cos(ang + 0.42) * 9 * cam.z, hy - Math.sin(ang + 0.42) * 9 * cam.z);
    ctx.closePath();
    ctx.fill();
    /* type label at midpoint */
    if (cam.z > 0.55) {
      const mx = (ax + bx) / 2, my = (ay + by) / 2;
      ctx.font = "10px monospace";
      ctx.fillStyle = "rgba(139,148,158,0.9)";
      ctx.textAlign = "center";
      ctx.fillText(edgeLabel(e), mx, my - 4);
    }
  });
  /* nodes */
  graphState.nodes.forEach((n) => {
    const p = graphState.pos.get(n.id);
    if (!p) return;
    const x = toX(p.x), y = toY(p.y), r = Math.max(3, p.r * cam.z);
    const label = labelOf(n);
    const col = colorFor(label);
    /* halo for dragged node */
    if (graphState.drag && graphState.drag.id === n.id) {
      ctx.beginPath(); ctx.arc(x, y, r + 5, 0, Math.PI * 2);
      ctx.strokeStyle = col; ctx.lineWidth = 1.5; ctx.stroke();
    }
    ctx.beginPath(); ctx.arc(x, y, r, 0, Math.PI * 2);
    ctx.fillStyle = col; ctx.globalAlpha = 0.88; ctx.fill(); ctx.globalAlpha = 1;
    ctx.strokeStyle = "rgba(255,255,255,0.35)"; ctx.lineWidth = 1; ctx.stroke();
    /* label */
    if (cam.z > 0.4) {
      ctx.font = (r > 16 ? "bold " : "") + "11px system-ui, sans-serif";
      ctx.fillStyle = "#e6edf3";
      ctx.textAlign = "center";
      ctx.fillText(nodeTitle(n), x, y + r + 13);
      ctx.fillStyle = "rgba(139,148,158,0.85)";
      ctx.fillText(label, x, y + r + 25);
    }
  });
}

/* ---------- canvas interactions ---------- */
function setupGraphCanvas() {
  const canvas = $("gql-canvas");
  if (!canvas || canvas.__gqlBound) return;
  canvas.__gqlBound = true;
  const cam = () => graphState.cam;
  const hitNode = (mx, my) => {
    for (let i = graphState.nodes.length - 1; i >= 0; i--) {
      const n = graphState.nodes[i];
      const p = graphState.pos.get(n.id);
      if (!p) continue;
      const x = p.x * cam().z + cam().x, y = p.y * cam().z + cam().y;
      const r = Math.max(3, p.r * cam().z) + 4;
      if (Math.abs(mx - x) <= r && Math.abs(my - y) <= r) return n.id;
    }
    return null;
  };
  canvas.addEventListener("pointerdown", (e) => {
    const rect = canvas.getBoundingClientRect();
    const mx = e.clientX - rect.left, my = e.clientY - rect.top;
    const id = hitNode(mx, my);
    if (id) {
      const p = graphState.pos.get(id);
      graphState.drag = { id, dx: mx - (p.x * cam().z + cam().x),
                          dy: my - (p.y * cam().z + cam().y) };
      canvas.setPointerCapture(e.pointerId);
    } else {
      graphState.pan = { sx: e.clientX, sy: e.clientY,
                         cx: cam().x, cy: cam().y };
      canvas.setPointerCapture(e.pointerId);
    }
    canvas.classList.add("dragging");
  });
  canvas.addEventListener("pointermove", (e) => {
    if (graphState.drag) {
      const rect = canvas.getBoundingClientRect();
      const mx = e.clientX - rect.left, my = e.clientY - rect.top;
      const p = graphState.pos.get(graphState.drag.id);
      p.x = (mx - graphState.drag.dx - cam().x) / cam().z;
      p.y = (my - graphState.drag.dy - cam().y) / cam().z;
      drawGraph();
    } else if (graphState.pan) {
      cam().x = graphState.pan.cx + (e.clientX - graphState.pan.sx);
      cam().y = graphState.pan.cy + (e.clientY - graphState.pan.sy);
      drawGraph();
    }
  });
  const endDrag = (e) => {
    graphState.drag = null; graphState.pan = null;
    canvas.classList.remove("dragging");
    try { canvas.releasePointerCapture(e.pointerId); } catch (err) { /* noop */ }
  };
  canvas.addEventListener("pointerup", endDrag);
  canvas.addEventListener("pointercancel", endDrag);
  canvas.addEventListener("wheel", (e) => {
    e.preventDefault();
    const rect = canvas.getBoundingClientRect();
    const mx = e.clientX - rect.left, my = e.clientY - rect.top;
    const z = cam().z * (e.deltaY < 0 ? 1.12 : 0.89);
    cam().z = Math.max(0.1, Math.min(6, z));
    /* zoom towards cursor */
    cam().x = mx - (mx - cam().x) * (cam().z / (e.deltaY < 0 ? 1.12 : 0.89));
    cam().y = my - (my - cam().y) * (cam().z / (e.deltaY < 0 ? 1.12 : 0.89));
    drawGraph();
  }, { passive: false });
  window.addEventListener("resize", () => { if (state.page === "graph") drawGraph(); });
}

/* ---------- graph page wiring ---------- */
function bindGraphPage() {
  const run = $("gql-run");
  if (run) run.addEventListener("click", runGraphql);
  const sample = $("gql-sample");
  if (sample) sample.addEventListener("click", () => {
    const el = $("gql-input");
    el.value = GQL_SAMPLE;
    graphState.query = GQL_SAMPLE;
    el.focus();
  });
  const fit = $("gql-fit");
  if (fit) fit.addEventListener("click", () => { layoutGraph(); drawGraph(); });
  $("seg-graph").addEventListener("click", () => { setSegActive("graph"); showGraphView(); });
  $("seg-json").addEventListener("click", () => { setSegActive("json"); showGraphView(); });
  const input = $("gql-input");
  if (input) input.addEventListener("keydown", (e) => {
    if ((e.metaKey || e.ctrlKey) && e.key === "Enter") runGraphql();
  });
  setupGraphCanvas();
  loadGraphSample();
}

bindGraphPage();
