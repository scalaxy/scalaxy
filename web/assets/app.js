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
  const titles = { overview: "Overview", data: "Data", cluster: "Cluster", console: "Console" };
  $("page-title").textContent = titles[name];
  if (name === "data") loadKeys();
  else if (name === "console") setTimeout(() => $("console-input").focus(), 50);
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
  const q = new URLSearchParams({ prefix, limit: state.limit, offset: state.offset });
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
    const d = await api("GET", "/api/keys/" + encodeURIComponent(key));
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
  try { await api("DELETE", "/api/keys/" + encodeURIComponent(key)); loadKeys(); }
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
    await api("PUT", "/api/keys/" + encodeURIComponent(key), { value });
    $("add-key").value = ""; $("add-value").value = "";
    closeModal();
    loadKeys();
  } catch (e) { alert(e.message); }
});

/* ---------- console ---------- */
$("btn-run").addEventListener("click", () => runQuery());
$("console-input").addEventListener("keydown", (e) => {
  // Enter runs the command; Shift+Enter inserts a newline
  if (e.key === "Enter" && !e.shiftKey && !e.isComposing) {
    e.preventDefault();
    runQuery();
  }
});

async function runQuery() {
  const command = $("console-input").value.trim();
  if (!command) return;
  const btn = $("btn-run");
  const out = $("console-output");
  btn.disabled = true;
  out.textContent = "running: " + command + "\n";
  try {
    const d = await api("POST", "/api/query", { command });
    out.textContent += (d.ok ? "" : "error: ") + (d.output || "");
    out.scrollTop = out.scrollHeight;
    if (state.page === "data") loadKeys();
    if (state.page === "overview") loadStatus();
  } catch (e) {
    out.textContent += "error: " + e.message;
  } finally {
    btn.disabled = false;
    $("console-input").focus();
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
