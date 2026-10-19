"use strict";
// Prose review. The server (scripts/prose_review.py) serves the Markdown
// document as JSON, relays questions to the review session and pushes
// updates over server-sent events. The panel, chips and chrome are shared
// with the assisted-review deck; the document view replaces its slides.
(() => {
  const MERMAID = {
    src: "https://cdnjs.cloudflare.com/ajax/libs/mermaid/11.15.0/mermaid.min.js",
    integrity: "sha384-yQ4mmBBT+vhTAwjFH0toJXNYJ6O4usWnt6EPIdWwrRvx2V/n5lXuDZQwQFeSFydF",
  };
  const KEY = "prose-review:" + location.pathname;

  const S = {
    doc: null,
    threads: {},
    status: { session: "away" },
    refs: [],
    seen: null, // the document version the reader last marked read
    changes: 0,
    banners: new Map(),
    askW: null,
  };

  // ---- small helpers ----

  const $ = (sel, el = document) => el.querySelector(sel);
  const esc = (s) =>
    String(s).replace(/[&<>"']/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" })[c]);

  function h(tag, attrs, ...kids) {
    const el = document.createElement(tag);
    for (const [k, v] of Object.entries(attrs || {})) {
      if (v == null || v === false) continue;
      if (k === "class") el.className = v;
      else if (k === "html") el.innerHTML = v;
      else if (k.startsWith("on")) el.addEventListener(k.slice(2), v);
      else el.setAttribute(k, v === true ? "" : v);
    }
    for (const kid of kids.flat()) {
      if (kid == null || kid === false) continue;
      el.append(kid instanceof Node ? kid : String(kid));
    }
    return el;
  }

  const store = {
    get(k, d) {
      try {
        const v = localStorage.getItem(KEY + ":" + k);
        return v === null ? d : JSON.parse(v);
      } catch {
        return d;
      }
    },
    set(k, v) {
      try {
        localStorage.setItem(KEY + ":" + k, JSON.stringify(v));
      } catch {}
    },
  };

  const debounce = (fn, ms) => {
    let t;
    return (...a) => {
      clearTimeout(t);
      t = setTimeout(() => fn(...a), ms);
    };
  };

  const clock = (ts) => new Date(ts * 1000).toLocaleTimeString([], { hour: "2-digit", minute: "2-digit" });
  const isDark = () => {
    const t = document.documentElement.dataset.theme;
    return t ? t === "dark" : matchMedia("(prefers-color-scheme: dark)").matches;
  };
  const narrow = () => matchMedia("(max-width: 960px)").matches;

  async function getJSON(path) {
    const r = await fetch(path, { cache: "no-store" });
    if (!r.ok) throw new Error(`${path}: ${r.status}`);
    return r.json();
  }

  async function post(path, body) {
    const r = await fetch(path, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(body || {}),
    });
    const data = await r.json().catch(() => ({}));
    if (!r.ok) throw new Error(data.error || r.statusText);
    return data;
  }

  // ---- markdown and highlighting ----

  if (window.hljs) hljs.configure({ ignoreUnescapedHTML: true });

  // Block elements carry their source lines (data-line through data-end,
  // 1-based), so a selection can be traced back to the Markdown and edits
  // can be matched block by block.
  const BLOCK_RULES = [
    "paragraph_open", "heading_open", "bullet_list_open", "ordered_list_open", "list_item_open",
    "blockquote_open", "table_open", "hr", "fence", "code_block",
  ];
  const mdit =
    window.markdownit &&
    markdownit({ html: true, linkify: true }).use((m) => {
      const def = (tokens, i, o, env, self) => self.renderToken(tokens, i, o);
      for (const rule of BLOCK_RULES) {
        const orig = m.renderer.rules[rule] || def;
        m.renderer.rules[rule] = (tokens, i, o, env, self) => {
          const t = tokens[i];
          if (t.map) {
            t.attrSet("data-line", String(t.map[0] + 1));
            t.attrSet("data-end", String(t.map[1]));
          }
          return orig(tokens, i, o, env, self);
        };
      }
    });

  function md(src) {
    const s = String(src || "");
    if (mdit && window.DOMPurify) return DOMPurify.sanitize(mdit.render(s));
    return s
      .split(/\n{2,}/)
      .map((p) => `<p>${esc(p).replace(/\n/g, "<br>")}</p>`)
      .join("");
  }

  function mdEl(src, cls = "prose") {
    const el = h("div", { class: cls, html: md(src) });
    for (const a of el.querySelectorAll("a[href]")) {
      if (/^https?:/.test(a.getAttribute("href"))) {
        a.target = "_blank";
        a.rel = "noopener noreferrer";
      }
    }
    if (window.hljs) {
      for (const c of el.querySelectorAll("pre code:not(.language-mermaid)")) {
        try {
          hljs.highlightElement(c);
        } catch {}
      }
    }
    return el;
  }

  // ---- diagrams (```mermaid fences) ----

  let mermaidReady = null;
  let mermaidN = 0;

  function loadMermaid() {
    mermaidReady ||= new Promise((resolve, reject) => {
      const s = document.createElement("script");
      Object.assign(s, { src: MERMAID.src, integrity: MERMAID.integrity, crossOrigin: "anonymous" });
      s.onload = () => resolve(window.mermaid);
      s.onerror = () => reject(new Error("could not load the diagram library"));
      document.head.append(s);
    });
    return mermaidReady;
  }

  async function mermaidInto(el, src) {
    el.replaceChildren(h("span", { class: "thread-empty" }, "Drawing the diagram…"));
    try {
      const m = await loadMermaid();
      const css = getComputedStyle(document.documentElement);
      const v = (name) => css.getPropertyValue(name).trim();
      m.initialize({
        startOnLoad: false,
        securityLevel: "strict",
        theme: "base",
        darkMode: isDark(),
        fontFamily: getComputedStyle(document.body).fontFamily,
        sequence: { width: 120, actorMargin: 36, boxMargin: 8, messageMargin: 32, mirrorActors: false },
        themeVariables: {
          background: v("--panel"),
          primaryColor: v("--panel"),
          primaryTextColor: v("--ink"),
          primaryBorderColor: v("--ink-3"),
          secondaryColor: v("--accent-wash"),
          secondaryBorderColor: v("--accent"),
          tertiaryColor: v("--sunk"),
          tertiaryBorderColor: v("--rule"),
          lineColor: v("--ink-3"),
          textColor: v("--ink"),
          mainBkg: v("--panel"),
          nodeBorder: v("--ink-3"),
          clusterBkg: v("--sunk"),
          clusterBorder: v("--rule"),
          edgeLabelBackground: v("--panel"),
          noteBkgColor: v("--sel-row"),
          noteBorderColor: v("--sel-edge"),
          noteTextColor: v("--ink"),
          actorBkg: v("--panel"),
          actorBorder: v("--ink-3"),
          signalColor: v("--ink-2"),
          fontSize: "15px",
        },
      });
      const { svg } = await m.render(`mmd-${++mermaidN}`, src);
      el.innerHTML = svg;
      fitDiagram(el.querySelector("svg"), el);
    } catch (e) {
      el.replaceChildren(h("pre", { class: "diagram-error" }, `The diagram failed to draw: ${e.message || e}\n\n${src}`));
    }
  }

  // Mermaid scales a diagram down to the column, which can leave its text
  // unreadably small. Shrink to at most MIN_SCALE; wider diagrams scroll.
  const MIN_SCALE = 0.9;
  function fitDiagram(svg, box) {
    if (!svg) return;
    const natural = parseFloat(svg.style.maxWidth) || svg.viewBox?.baseVal?.width;
    if (!natural || !box.clientWidth) return;
    const scale = Math.max(MIN_SCALE, Math.min(1, box.clientWidth / natural));
    svg.style.maxWidth = "none";
    svg.setAttribute("width", Math.round(natural * scale));
    svg.removeAttribute("height");
    if (natural * scale > box.clientWidth) box.style.justifyItems = "start";
  }

  function drawDiagrams(root) {
    for (const code of root.querySelectorAll("pre > code.language-mermaid")) {
      const body = h("div", { class: "diagram-body" });
      const fig = h("figure", { class: "diagram", "data-line": code.dataset.line, "data-end": code.dataset.end }, body);
      code.parentElement.replaceWith(fig);
      mermaidInto(body, code.textContent);
    }
  }

  // ---- document state ----

  const messages = () => S.threads.doc?.messages || [];
  const pending = (m) => m.role === "user" && (m.state === "queued" || m.state === "claimed");
  // Source lines, 1-based in the markup; a final newline does not add a line.
  const lines = () => {
    const ls = S.doc.text.split("\n");
    if (ls.length && ls.at(-1) === "") ls.pop();
    return ls;
  };

  async function loadDoc() {
    const doc = await getJSON("api/doc");
    S.doc = doc;
    if (doc.status) S.status = doc.status;
    document.title = doc.title;
    $("#doc-title").textContent = doc.title;
    $("#doc-title").title = doc.path;
    const about = [doc.subtitle, doc.file].filter(Boolean);
    $("#doc-scope").textContent = about.join(" · ");
    $("#doc-scope").title = doc.path;
    $("#toc-about").replaceChildren(...about.map((t) => h("p", {}, t)));
    $("#ask-sub").textContent = doc.file;
  }

  async function loadThreads({ notify = false } = {}) {
    const before = new Set(messages().map((m) => m.id));
    S.threads = await getJSON("api/threads");
    if (notify && narrow() && !$("#ask").classList.contains("open")) {
      const fresh = messages().filter((m) => m.role === "assistant" && !before.has(m.id));
      if (fresh.length) toast("The review session answered.", { label: "Show", run: () => $("#ask").classList.add("open") });
    }
  }

  // ---- rendering ----

  function renderDoc({ keepScroll = true, announce = false } = {}) {
    const el = $("#doc");
    const top = el.scrollTop;
    const inner = mdEl(S.doc.text, "doc-inner prose");
    drawDiagrams(inner);
    for (const hd of inner.querySelectorAll("h1, h2, h3, h4, h5, h6")) if (hd.dataset.line) hd.id = `L${hd.dataset.line}`;
    el.replaceChildren(inner);
    paintQuoted();
    el.scrollTop = keepScroll ? top : 0;
    renderToc();
    markChanges({ announce });
  }

  function renderToc() {
    const d = S.doc;
    const words = d.text.split(/\s+/).filter(Boolean).length;
    $("#toc-cover").textContent = `${lines().length} lines · about ${words} words`;
    const list = $("#toc-list");
    list.replaceChildren();
    for (const hd of document.querySelectorAll("#doc h1, #doc h2, #doc h3, #doc h4, #doc h5, #doc h6")) {
      const li = h(
        "li",
        { class: hd.classList.contains("changed") ? "changed-h" : "" },
        h(
          "button",
          { type: "button", onclick: () => (closeToc(), reveal(hd)) },
          h("span", {}, hd.textContent),
          h("span", { class: "toc-num" }, hd.dataset.line ? `L${hd.dataset.line}` : ""),
        ),
      );
      li.style.setProperty("--lvl", hd.tagName.slice(1));
      list.append(li);
    }
    if (!list.children.length) list.append(h("li", { class: "thread-empty" }, "No headings."));
  }

  function reveal(el) {
    el.scrollIntoView({ block: "start", behavior: "smooth" });
    el.classList.add("target");
    setTimeout(() => el.classList.remove("target"), 1600);
  }

  // ---- edits since the reader last marked the document read ----
  //
  // The server keeps recent versions of the text. The page compares the
  // version the reader last marked read with the current one, block by
  // block, and marks each changed block; clicking one shows what it replaced.

  // Units are the finest blocks with source lines: a paragraph, heading,
  // code block, table or rule, a tight list item, or the lead line of a list
  // item that holds a nested list.
  function units(root, src) {
    const out = [];
    for (const el of root.querySelectorAll("[data-line]")) {
      const lo = +el.dataset.line;
      let hi = +el.dataset.end;
      const kid = el.querySelector("[data-line]");
      if (kid) {
        if (el.tagName !== "LI") continue;
        hi = Math.min(hi, +kid.dataset.line - 1);
        if (hi < lo) continue;
      }
      out.push({ el, lo, hi, text: src.slice(lo - 1, hi).join("\n").trim() });
    }
    return out;
  }

  // Edit script between two sequences as [op, oldIndex, newIndex] triples:
  // common ends are trimmed, the middle goes through a plain LCS table
  // (sizes here are blocks or words).
  function diffSeq(a, b) {
    let s = 0;
    while (s < a.length && s < b.length && a[s] === b[s]) s++;
    let e = 0;
    while (e < a.length - s && e < b.length - s && a[a.length - 1 - e] === b[b.length - 1 - e]) e++;
    const A = a.slice(s, a.length - e);
    const B = b.slice(s, b.length - e);
    const ops = [];
    for (let i = 0; i < s; i++) ops.push(["=", i, i]);
    const n = A.length;
    const m = B.length;
    if (n * m > 1_000_000) {
      for (let i = 0; i < n; i++) ops.push(["-", s + i, null]);
      for (let j = 0; j < m; j++) ops.push(["+", null, s + j]);
    } else {
      const L = Array.from({ length: n + 1 }, () => new Uint32Array(m + 1));
      for (let i = n - 1; i >= 0; i--) {
        for (let j = m - 1; j >= 0; j--) L[i][j] = A[i] === B[j] ? L[i + 1][j + 1] + 1 : Math.max(L[i + 1][j], L[i][j + 1]);
      }
      let i = 0;
      let j = 0;
      while (i < n && j < m) {
        if (A[i] === B[j]) ops.push(["=", s + i++, s + j++]);
        else if (L[i + 1][j] >= L[i][j + 1]) ops.push(["-", s + i++, null]);
        else ops.push(["+", null, s + j++]);
      }
      while (i < n) ops.push(["-", s + i++, null]);
      while (j < m) ops.push(["+", null, s + j++]);
    }
    for (let k = 0; k < e; k++) ops.push(["=", a.length - e + k, b.length - e + k]);
    return ops;
  }

  // Old and new text of a block, with changed words marked.
  function wordDiff(a, b) {
    const tok = (s) => s.match(/\s+|\S+/g) || [];
    const A = tok(a);
    const B = tok(b);
    const frag = document.createDocumentFragment();
    let run = null;
    let kind = null;
    const flush = () => {
      if (run == null) return;
      frag.append(kind ? h(kind, {}, run) : run);
      run = null;
    };
    for (const [op, i, j] of diffSeq(A, B)) {
      const k = op === "=" ? null : op === "-" ? "del" : "ins";
      if (k !== kind) {
        flush();
        kind = k;
      }
      run = (run ?? "") + (op === "+" ? B[j] : A[i]);
    }
    flush();
    return frag;
  }

  function prevEl(oldText, newText) {
    const who = oldText && newText ? "Before this edit" : oldText ? "Removed" : "Added";
    return h("div", { class: "prev" }, h("span", { class: "who" }, who), wordDiff(oldText || "", newText || ""));
  }

  function togglePrev(el, oldText, newText) {
    const next = el.nextElementSibling;
    if (next?.classList.contains("prev")) {
      next.remove();
      el.classList.remove("open");
      return;
    }
    el.after(prevEl(oldText, newText));
    el.classList.add("open");
  }

  // A block the reader can open to see what it replaced.
  function markBlock(el, oldText, newText) {
    el.classList.add("changed");
    el.tabIndex = 0;
    el.title = `${oldText ? "Edited" : "Added"} since you last marked the document read · click to ${oldText ? "compare" : "confirm"}`;
    el.addEventListener("click", (e) => {
      if (e.target.closest("a, button, input, label, .prev") || !getSelection().isCollapsed) return;
      togglePrev(el, oldText, newText);
    });
    el.addEventListener("keydown", (e) => {
      if (e.target === el && (e.key === "Enter" || e.key === " ")) {
        e.preventDefault();
        togglePrev(el, oldText, newText);
      }
    });
  }

  // Top-level ancestor inside the document column, where a marker can sit.
  const topOf = (el) => {
    let cur = el;
    while (cur.parentElement && !cur.parentElement.classList.contains("doc-inner")) cur = cur.parentElement;
    return cur;
  };

  function clearMarks() {
    for (const el of document.querySelectorAll("#doc .prev, #doc .removed")) el.remove();
    for (const el of document.querySelectorAll("#doc .changed")) {
      el.classList.remove("changed", "open");
      el.removeAttribute("tabindex");
      el.removeAttribute("title");
    }
  }

  function setChanges(n) {
    S.changes = n;
    const btn = $("#changes-btn");
    btn.hidden = !n;
    btn.textContent = n ? `${n} edit${n === 1 ? "" : "s"}` : "";
  }

  function firstChange() {
    return $("#doc .changed, #doc .removed");
  }

  async function markChanges({ announce = false } = {}) {
    clearMarks();
    const v = S.doc.version;
    if (S.seen == null || S.seen > v) {
      // First visit, or the server restarted and its numbering reset.
      markRead();
      return;
    }
    if (S.seen === v) return setChanges(0);
    let old;
    try {
      old = (await getJSON(`api/version/${Math.max(S.seen, S.doc.oldest)}`)).text;
    } catch {
      return setChanges(0);
    }
    if (S.doc.version !== v) return; // a newer version arrived meanwhile
    const inner = $("#doc .doc-inner");
    if (!inner) return;
    const A = units(h("div", { html: md(old) }), old.split("\n"));
    const B = units(inner, lines());
    const ops = diffSeq(A.map((u) => u.text), B.map((u) => u.text));
    // Group runs of deletions and insertions into hunks; a hunk remembers the
    // unchanged block that follows it, where a marker for pure deletions goes.
    const hunks = [];
    let cur = null;
    for (const [op, i, j] of ops) {
      if (op === "=") {
        if (cur) cur.next = B[j].el;
        cur = null;
        continue;
      }
      if (!cur) hunks.push((cur = { dels: [], adds: [], next: null }));
      if (op === "-") cur.dels.push(A[i]);
      else cur.adds.push(B[j]);
    }
    for (const hk of hunks) {
      const oldText = hk.dels.map((u) => u.text).join("\n\n");
      if (hk.adds.length) {
        const newText = hk.adds.map((u) => u.text).join("\n\n");
        for (const u of hk.adds) markBlock(u.el, oldText, newText);
      } else {
        const first = hk.dels[0].text.replace(/\s+/g, " ").slice(0, 60);
        const marker = h("button", { type: "button", class: "removed", title: "Click to show the removed text" }, `Removed: “${first}${hk.dels[0].text.length > 60 ? "…" : ""}”`);
        marker.addEventListener("click", () => togglePrev(marker, oldText, ""));
        if (hk.next) topOf(hk.next).before(marker);
        else inner.append(marker);
      }
    }
    const k = hunks.length;
    setChanges(k);
    renderToc();
    if (announce && k) toast(`The review session edited the document: ${k} change${k === 1 ? "" : "s"}.`, { label: "Show", run: () => firstChange() && reveal(firstChange()) });
  }

  function markRead() {
    S.seen = S.doc.version;
    store.set("seen", { path: S.doc.path, version: S.seen });
    clearMarks();
    setChanges(0);
    renderToc();
  }

  // ---- quoting a passage ----

  const snippet = (text) => text.replace(/[*`_]+/g, "").replace(/\s+/g, " ").trim().slice(0, 60);

  function passage(text, lo, hi) {
    const span = lo ? `L${lo}${lo === hi ? "" : `–${hi}`} ` : "";
    return { kind: "passage", text, lo: lo || null, hi: hi || null, label: span + snippet(text) };
  }

  function sourceHref(lo, hi) {
    const t = S.doc?.sourceUrl;
    if (!t || !lo) return null;
    return t.replace("{path}", encodeURI(S.doc.file)).replace("{line}", lo).replace("{end}", hi);
  }

  // The block that holds a point of the selection, and its source lines.
  function blockAt(node) {
    const el = node.nodeType === 1 ? node : node.parentElement;
    if (!el || !el.closest("#doc") || el.closest(".prev, .removed")) return undefined;
    return el.closest("[data-line]");
  }

  function docSelection() {
    const sel = getSelection();
    if (!sel || sel.isCollapsed || !sel.rangeCount) return null;
    const range = sel.getRangeAt(0);
    const first = blockAt(range.startContainer);
    const last = blockAt(range.endContainer);
    if (first === undefined || last === undefined) return null;
    const text = sel.toString().trim();
    if (text.length < 2) return null;
    let lo = first ? +first.dataset.line : 0;
    let hi = last ? +last.dataset.end : 0;
    // A selection that ends at the very start of a block does not include it.
    if (last && last !== first && range.endOffset === 0) hi = Math.max(lo, +last.dataset.line - 1);
    if (lo && hi < lo) hi = lo;
    return { text, lo, hi, rect: range.getBoundingClientRect() };
  }

  const quoteBtn = h("button", { type: "button", class: "primary small" }, "Ask about this text");
  const ctxLink = h("a", { target: "_blank", rel: "noopener noreferrer", hidden: true }, "View in context");
  const quotePop = h("div", { class: "quote-pop", hidden: true }, quoteBtn, ctxLink);
  document.body.append(quotePop);

  document.addEventListener(
    "selectionchange",
    debounce(() => {
      const s = docSelection();
      quotePop.hidden = !s;
      if (!s) return;
      const href = sourceHref(s.lo, s.hi);
      ctxLink.hidden = !href;
      if (href) ctxLink.href = href;
      const top = Math.min(innerHeight - 48, s.rect.bottom + 8);
      const left = Math.max(8, Math.min(innerWidth - quotePop.offsetWidth - 8, s.rect.left));
      Object.assign(quotePop.style, { top: `${top}px`, left: `${left}px` });
    }, 120),
  );
  quotePop.addEventListener("mousedown", (e) => e.target === quoteBtn && e.preventDefault());
  quoteBtn.addEventListener("click", () => {
    const s = docSelection();
    quotePop.hidden = true;
    if (!s) return;
    getSelection().removeAllRanges();
    addRef(passage(s.text.slice(0, 2000), s.lo, s.hi));
  });

  function addRef(ref) {
    const same = (r) => JSON.stringify(r) === JSON.stringify(ref);
    if (!S.refs.some(same)) S.refs.push(ref);
    saveDraft();
    renderChips();
    paintQuoted();
    if (narrow()) $("#ask").classList.add("open");
    $("#question").focus();
  }

  // Leaf blocks whose lines overlap the quoted range.
  function blocksIn(lo, hi) {
    if (!lo) return [];
    return [...document.querySelectorAll("#doc [data-line]")].filter((el) => {
      if (el.querySelector("[data-line]") && el.tagName !== "LI") return false;
      const a = +el.dataset.line;
      const b = el.tagName === "LI" && el.querySelector("[data-line]") ? a : +el.dataset.end;
      return a <= hi && b >= lo;
    });
  }

  function paintQuoted() {
    for (const el of document.querySelectorAll("#doc .quoted")) el.classList.remove("quoted");
    for (const r of S.refs) for (const el of blocksIn(r.lo, r.hi)) el.classList.add("quoted");
  }

  function flashRef(r) {
    const els = blocksIn(r.lo, r.hi);
    if (!els.length) return;
    els[0].scrollIntoView({ block: "center", behavior: "smooth" });
    for (const el of els) el.classList.add("quoted");
    setTimeout(paintQuoted, 1600);
  }

  // ---- questions ----

  function renderThread() {
    const msgs = messages();
    $("#clear-btn").hidden = !msgs.length || msgs.some(pending);
    const t = $("#thread");
    t.replaceChildren();
    if (S.status.session === "away") {
      t.append(
        h(
          "p",
          { class: "offline-note" },
          "The review session is not listening right now. Questions wait until it is; to resume, ask it in the terminal to keep listening.",
        ),
      );
    }
    if (!msgs.length) {
      t.append(
        h(
          "div",
          { class: "thread-empty" },
          h("p", {}, "No questions yet. Select text in the document to quote it, then ask about it or ask for a change."),
          h("p", {}, "The review session that opened this document answers here and makes the edits you ask for; edited passages are marked in the document."),
        ),
      );
    }
    for (const m of msgs) t.append(m.role === "user" ? questionEl(m) : answerEl(m));
    t.scrollTop = t.scrollHeight;
    renderChips();
    const n = msgs.filter((m) => m.role === "user").length;
    $("#ask-count").textContent = n ? `(${n})` : "";
  }

  function questionEl(m) {
    const state = {
      queued: [h("span", {}, "Waiting for the review session · ", h("span", { "data-since": m.at })), h("button", { type: "button", class: "ghost small", onclick: () => withdraw(m) }, "Withdraw")],
      claimed: [h("span", { class: "dots" }, "The review session is answering"), h("span", { "data-since": m.at })],
      withdrawn: [h("span", {}, "Withdrawn")],
    }[m.state];
    return h(
      "div",
      { class: `msg msg-q ${m.state || ""}` },
      h("div", { class: "who" }, `You · ${clock(m.at)}`),
      m.refs?.length
        ? h("div", { class: "chips" }, m.refs.map((r) => h("span", { class: "chip static", title: r.text || r.label, onclick: () => flashRef(r) }, h("span", {}, r.label))))
        : null,
      m.text ? h("div", { class: "text" }, m.text) : null,
      state ? h("div", { class: "msg-state" }, state) : null,
    );
  }

  function answerEl(m) {
    return h("div", { class: "msg msg-a" }, h("div", { class: "who" }, `Review session · ${clock(m.at)}`), mdEl(m.text, "body prose"));
  }

  function tickElapsed() {
    for (const el of document.querySelectorAll("[data-since]")) {
      const s = Math.max(0, Math.round(Date.now() / 1000 - +el.dataset.since));
      el.textContent = s < 60 ? `${s} s` : `${Math.floor(s / 60)} min`;
    }
  }

  async function withdraw(m) {
    try {
      await post("api/withdraw", { id: m.id });
    } catch (e) {
      toast(e.message);
    }
  }

  function renderChips() {
    const chips = $("#chips");
    chips.replaceChildren(
      ...S.refs.map((r, i) =>
        h(
          "span",
          { class: "chip", title: r.text || r.label },
          h("span", {}, r.label),
          h("button", {
            type: "button",
            "aria-label": `Remove ${r.label}`,
            onclick: () => {
              S.refs.splice(i, 1);
              saveDraft();
              renderChips();
              paintQuoted();
            },
          }, "×"),
        ),
      ),
    );
    updateSend();
  }

  function updateSend() {
    $("#send").disabled = !$("#question").value.trim() && !S.refs.length;
  }

  function saveDraft() {
    store.set("draft", { text: $("#question").value, refs: S.refs });
  }

  function loadDraft() {
    const d = store.get("draft", { text: "", refs: [] });
    $("#question").value = d.text || "";
    S.refs = d.refs || [];
  }

  async function send() {
    const text = $("#question").value.trim();
    if (!text && !S.refs.length) return;
    $("#send").disabled = true;
    try {
      const { message } = await post("api/ask", { text, refs: S.refs });
      (S.threads.doc ||= { messages: [] }).messages.push(message);
      $("#question").value = "";
      S.refs = [];
      saveDraft();
      paintQuoted();
      renderThread();
    } catch (e) {
      toast(`Could not send the question: ${e.message}`);
    } finally {
      updateSend();
    }
  }

  // ---- contents and chrome ----

  function openToc() {
    $("#toc").hidden = false;
    $("#toc-btn").setAttribute("aria-expanded", "true");
    $("#toc-list button")?.focus();
  }
  function closeToc() {
    $("#toc").hidden = true;
    $("#toc-btn").setAttribute("aria-expanded", "false");
  }

  const THEMES = ["system", "light", "dark"];
  function applyTheme(t) {
    if (t === "system") delete document.documentElement.dataset.theme;
    else document.documentElement.dataset.theme = t;
    $("#theme-btn").title = `Theme: ${t}`;
    store.set("theme", t);
  }

  // ---- question panel width ----

  // The docked panel's width lives in --ask-w; the stylesheet's default
  // applies until the reader drags the handle. Null means "use the default".
  const ASK_MIN = 280;
  const askMax = () => Math.max(ASK_MIN, Math.min(innerWidth - 480, 900));
  const askWidth = () => $("#ask").getBoundingClientRect().width;

  function setAskWidth(w, { save = true } = {}) {
    S.askW = w == null ? null : Math.round(Math.max(ASK_MIN, Math.min(askMax(), w)));
    const root = document.documentElement.style;
    if (S.askW == null) root.removeProperty("--ask-w");
    else root.setProperty("--ask-w", `${S.askW}px`);
    const grip = $("#ask-grip");
    grip.setAttribute("aria-valuemin", ASK_MIN);
    grip.setAttribute("aria-valuemax", askMax());
    grip.setAttribute("aria-valuenow", Math.round(askWidth()));
    if (save) store.set("ask-width", S.askW);
  }

  function bindAskGrip() {
    const grip = $("#ask-grip");
    let resize = null;
    const end = () => {
      if (!resize) return;
      resize = null;
      document.body.classList.remove("resizing");
      store.set("ask-width", S.askW);
    };
    grip.addEventListener("pointerdown", (e) => {
      if (e.button !== 0) return;
      e.preventDefault();
      resize = { x: e.clientX, w: askWidth() };
      grip.setPointerCapture(e.pointerId);
      document.body.classList.add("resizing");
    });
    grip.addEventListener("pointermove", (e) => resize && setAskWidth(resize.w + resize.x - e.clientX, { save: false }));
    grip.addEventListener("pointerup", end);
    grip.addEventListener("pointercancel", end);
    grip.addEventListener("dblclick", () => setAskWidth(null));
    grip.addEventListener("keydown", (e) => {
      if (e.altKey || e.ctrlKey || e.metaKey) return;
      const step = e.shiftKey ? 64 : 16;
      const cur = askWidth();
      const next = { ArrowLeft: cur + step, ArrowRight: cur - step, Home: askMax(), End: ASK_MIN, Enter: null }[e.key];
      if (next === undefined) return;
      e.preventDefault();
      setAskWidth(next);
    });
    window.addEventListener(
      "resize",
      debounce(() => S.askW != null && setAskWidth(S.askW, { save: false }), 100),
    );
  }

  const PRESENCE = {
    listening: ["Listening", "The review session is waiting for questions."],
    answering: ["Answering", "The review session is answering a question; new ones wait their turn."],
    away: ["Not listening", "The review session is not waiting for questions; they are queued until it is."],
  };

  function setStatus(st) {
    if (!st || !PRESENCE[st.session]) return;
    const changed = st.session !== S.status.session;
    S.status = st;
    const pill = $("#listen-pill");
    pill.dataset.on = String(st.session !== "away");
    [pill.textContent, pill.title] = PRESENCE[st.session];
    if (changed && S.doc) renderThread();
  }

  function banner(key, text, details) {
    if (text) S.banners.set(key, { text, details });
    else S.banners.delete(key);
    const el = $("#banner");
    el.hidden = !S.banners.size;
    el.replaceChildren(
      ...[...S.banners.values()].map((b) => h("div", {}, b.text, b.details?.length ? h("pre", {}, b.details.join("\n")) : null)),
    );
  }

  function toast(text, action) {
    const el = h(
      "div",
      { class: "toast" },
      h("span", {}, text),
      action ? h("button", { type: "button", class: "small", onclick: () => (action.run(), el.remove()) }, action.label) : null,
    );
    $("#toasts").append(el);
    setTimeout(() => el.remove(), action ? 9000 : 4500);
  }

  async function refreshDoc() {
    try {
      await loadDoc();
    } catch (e) {
      return banner("doc", `Could not reload the document: ${e.message}`);
    }
    banner("doc", null);
    renderDoc({ announce: true });
  }

  function connect() {
    const es = new EventSource("api/events");
    let failures = 0;
    const threadsChanged = debounce(async () => {
      try {
        await loadThreads({ notify: true });
        renderThread();
      } catch {}
    }, 150);
    es.addEventListener("hello", (e) => {
      const d = JSON.parse(e.data);
      if (failures) {
        banner("conn", null);
        threadsChanged();
      }
      failures = 0;
      setStatus(d.status);
      if (S.doc && d.version !== S.doc.version) refreshDoc();
    });
    es.addEventListener("doc", () => refreshDoc());
    es.addEventListener("doc-error", (e) =>
      banner("doc", "The document cannot be read right now, so this page still shows the previous version.", JSON.parse(e.data).errors),
    );
    es.addEventListener("thread", threadsChanged);
    es.addEventListener("status", (e) => setStatus(JSON.parse(e.data)));
    es.onerror = () => {
      failures += 1;
      setStatus({ session: "away" });
      if (failures > 1) banner("conn", "Lost the connection to the review server. Reconnecting…");
    };
  }

  // A button that asks once before acting.
  function confirmable(btn, label, ask, run) {
    btn.onclick = async () => {
      if (btn.dataset.confirm !== "1") {
        btn.dataset.confirm = "1";
        btn.textContent = ask;
        setTimeout(() => ((btn.dataset.confirm = ""), (btn.textContent = label)), 4000);
        return;
      }
      btn.dataset.confirm = "";
      btn.textContent = label;
      try {
        await run();
      } catch (err) {
        toast(err.message);
      }
    };
  }

  function bindUI() {
    $("#toc-btn").onclick = () => ($("#toc").hidden ? openToc() : closeToc());
    $("#toc-close").onclick = closeToc;
    $("#doc-scope").onclick = openToc;
    $("#toc").addEventListener("click", (e) => e.target.id === "toc" && closeToc());
    $("#theme-btn").onclick = () => {
      const cur = store.get("theme", "system");
      applyTheme(THEMES[(THEMES.indexOf(cur) + 1) % THEMES.length]);
      if (S.doc) renderDoc();
    };
    $("#changes-btn").onclick = markRead;
    confirmable($("#finish-btn"), "Finish", "Finish the review?", async () => {
      await post("api/finish", {});
      toast("Told the review session you are done.");
    });
    $("#ask-open").onclick = () => $("#ask").classList.add("open");
    $("#ask-close").onclick = () => $("#ask").classList.remove("open");
    bindAskGrip();
    confirmable($("#clear-btn"), "Clear", "Clear these questions?", async () => {
      await post("api/clear", {});
      S.threads = {};
      renderThread();
    });
    $("#doc").addEventListener("scroll", () => (quotePop.hidden = true));
    const q = $("#question");
    q.addEventListener("input", () => (updateSend(), saveDraft()));
    q.addEventListener("keydown", (e) => {
      if (e.key === "Enter" && !e.shiftKey && !e.isComposing) {
        e.preventDefault();
        send();
      }
    });
    $("#composer").addEventListener("submit", (e) => (e.preventDefault(), send()));
    matchMedia("(prefers-color-scheme: dark)").addEventListener("change", () => S.doc && renderDoc());
    document.addEventListener("keydown", (e) => {
      if (e.defaultPrevented || e.altKey || e.ctrlKey || e.metaKey) return;
      if (e.key === "Escape") {
        if (!$("#toc").hidden) return closeToc();
        quotePop.hidden = true;
        $("#ask").classList.remove("open");
        return;
      }
      if (e.target.closest?.("textarea, input, [contenteditable]")) return;
      if (e.key === "/") {
        e.preventDefault();
        if (narrow()) $("#ask").classList.add("open");
        $("#question").focus();
      }
    });
  }

  async function boot() {
    applyTheme(store.get("theme", "system"));
    setAskWidth(store.get("ask-width", null), { save: false });
    bindUI();
    try {
      await loadDoc();
      await loadThreads();
    } catch (e) {
      $("#doc-title").textContent = "The review server is not reachable";
      banner("doc", `Could not load the document: ${e.message}. Ask the review session to start the server again.`);
      return;
    }
    const seen = store.get("seen", null);
    S.seen = seen && seen.path === S.doc.path ? seen.version : null;
    loadDraft();
    renderDoc({ keepScroll: false });
    renderThread();
    setStatus(S.status);
    connect();
    setInterval(tickElapsed, 1000);
  }

  boot();
})();
