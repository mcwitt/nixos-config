"use strict";
// Page chrome shared by walkthrough's deck (deck.js, which walkthrough-pr
// extends) and prose-review's document (prose.js): helpers, toasts and
// banners, the theme, the question panel's drag handles, the session's
// presence and Mermaid diagrams. Each page loads this script first and
// reads window.UI; the markup it expects (#banner, #toasts, #theme-btn,
// #listen-pill, #ask, #ask-grip, #question, #question-grip) is in each
// page's index.html.
window.UI = (() => {
  const MERMAID = {
    src: "https://cdnjs.cloudflare.com/ajax/libs/mermaid/11.15.0/mermaid.min.js",
    integrity: "sha384-yQ4mmBBT+vhTAwjFH0toJXNYJ6O4usWnt6EPIdWwrRvx2V/n5lXuDZQwQFeSFydF",
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

  // localStorage under a page's prefix; a browser that refuses storage gets the defaults.
  const storage = (prefix) => ({
    get(k, d) {
      try {
        const v = localStorage.getItem(prefix + ":" + k);
        return v === null ? d : JSON.parse(v);
      } catch {
        return d;
      }
    },
    set(k, v) {
      try {
        localStorage.setItem(prefix + ":" + k, JSON.stringify(v));
      } catch {}
    },
  });

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

  // ---- toasts and banners ----

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

  // One line per key under the top bar; details fold away under the line.
  const banners = new Map();
  function banner(key, text, details) {
    if (text) banners.set(key, { text, details });
    else banners.delete(key);
    const el = $("#banner");
    el.hidden = !banners.size;
    el.replaceChildren(
      ...[...banners.values()].map((b) =>
        h("div", {}, b.text, b.details?.length ? h("details", {}, h("summary", {}, "Details"), h("pre", {}, b.details.join("\n"))) : null),
      ),
    );
  }

  // ---- theme ----

  const THEMES = ["system", "light", "dark"];
  function applyTheme(store, t) {
    if (t === "system") delete document.documentElement.dataset.theme;
    else document.documentElement.dataset.theme = t;
    $("#theme-btn").title = `Theme: ${t}`;
    store.set("theme", t);
  }
  const nextTheme = (store) => applyTheme(store, THEMES[(THEMES.indexOf(store.get("theme", "system")) + 1) % THEMES.length]);

  // ---- resizing ----

  // A drag handle for the question panel's width or the question box's
  // height. Both grow away from the edge they sit on, leftward or upward;
  // arrow keys step, Home and End jump, Enter or a double click restores
  // the stylesheet's size. The size is remembered in the page's store.
  function resizable({ store, grip, key, axis, min, max, current, apply }) {
    let value = null;
    const set = (v, save = true) => {
      value = v == null ? null : Math.round(Math.max(min(), Math.min(max(), v)));
      apply(value);
      grip.setAttribute("aria-valuemin", min());
      grip.setAttribute("aria-valuemax", max());
      grip.setAttribute("aria-valuenow", Math.round(current()));
      if (save) store.set(key, value);
    };
    const away = (e) => -(axis === "x" ? e.clientX : e.clientY);
    let drag = null;
    const end = () => {
      if (!drag) return;
      drag = null;
      delete document.body.dataset.resizing;
      store.set(key, value);
    };
    grip.addEventListener("pointerdown", (e) => {
      if (e.button !== 0) return;
      e.preventDefault();
      drag = { from: away(e), size: current() };
      grip.setPointerCapture(e.pointerId);
      document.body.dataset.resizing = axis;
    });
    grip.addEventListener("pointermove", (e) => drag && set(drag.size + away(e) - drag.from, false));
    grip.addEventListener("pointerup", end);
    grip.addEventListener("pointercancel", end);
    grip.addEventListener("dblclick", () => set(null));
    const [grow, shrink] = axis === "x" ? ["ArrowLeft", "ArrowRight"] : ["ArrowUp", "ArrowDown"];
    grip.addEventListener("keydown", (e) => {
      if (e.altKey || e.ctrlKey || e.metaKey) return;
      const step = e.shiftKey ? 64 : 16;
      const next = { [grow]: current() + step, [shrink]: current() - step, Home: max(), End: min(), Enter: null }[e.key];
      if (next === undefined) return;
      e.preventDefault();
      set(next);
    });
    window.addEventListener("resize", debounce(() => value != null && set(value, false), 100));
    set(store.get(key, null), false);
  }

  // The question panel's width (--ask-w) and the question box's height.
  function bindResizers(store) {
    const root = document.documentElement.style;
    resizable({
      store,
      grip: $("#ask-grip"),
      key: "ask-width",
      axis: "x",
      min: () => 280,
      max: () => Math.max(280, Math.min(innerWidth - 480, 900)),
      current: () => $("#ask").getBoundingClientRect().width,
      apply: (w) => (w == null ? root.removeProperty("--ask-w") : root.setProperty("--ask-w", `${w}px`)),
    });
    const q = $("#question");
    resizable({
      store,
      grip: $("#question-grip"),
      key: "question-height",
      axis: "y",
      min: () => 64,
      max: () => Math.round(innerHeight * 0.6),
      current: () => q.getBoundingClientRect().height,
      apply: (px) => (q.style.height = px == null ? "" : `${px}px`),
    });
  }

  // ---- the session's presence and the question thread ----

  const PRESENCE = {
    listening: ["Listening", (who) => `The ${who} is waiting for questions.`],
    answering: ["Answering", (who) => `The ${who} is answering a question; new ones wait their turn.`],
    working: ["Working", (who) => `The ${who} is working on something you asked for here; questions are queued until it listens again.`],
    away: ["Not listening", (who) => `The ${who} is not waiting for questions; they are queued until it is.`],
  };
  const knownPresence = (st) => !!st && st.session in PRESENCE;

  function showPresence(st, who) {
    const pill = $("#listen-pill");
    pill.dataset.on = String(st.session !== "away");
    pill.textContent = PRESENCE[st.session][0];
    pill.title = PRESENCE[st.session][1](who);
  }

  // The note at the top of the thread while questions cannot be answered.
  function presenceNote(st, who) {
    if (st.session === "away") {
      return h(
        "p",
        { class: "offline-note" },
        `The ${who} is not listening right now. Questions wait until it is; to resume, ask it in the terminal to keep listening.`,
      );
    }
    if (st.session === "working") {
      return h("p", { class: "offline-note" }, `The ${who} is working on something you asked for here. Questions wait until it listens again.`);
    }
    return null;
  }

  // Where a question stands, under the question.
  function questionState(m, who, withdraw) {
    const state = {
      queued: [h("span", {}, `Waiting for the ${who} · `, h("span", { "data-since": m.at })), h("button", { type: "button", class: "ghost small", onclick: () => withdraw(m) }, "Withdraw")],
      claimed: [h("span", { class: "ellipsis" }, `The ${who} is answering`), h("span", { "data-since": m.at })],
      withdrawn: [h("span", {}, "Withdrawn")],
    }[m.state];
    return state ? h("div", { class: "msg-state" }, state) : null;
  }

  function tickElapsed() {
    for (const el of document.querySelectorAll("[data-since]")) {
      const s = Math.max(0, Math.round(Date.now() / 1000 - +el.dataset.since));
      el.textContent = s < 60 ? `${s} s` : `${Math.floor(s / 60)} min`;
    }
  }

  // ---- Mermaid diagrams ----

  let mermaidReady = null;
  let mermaidN = 0;
  const LOAD_FAILED = "could not load the diagram library";

  function loadMermaid() {
    mermaidReady ||= new Promise((resolve, reject) => {
      const s = document.createElement("script");
      Object.assign(s, { src: MERMAID.src, integrity: MERMAID.integrity, crossOrigin: "anonymous" });
      s.onload = () => resolve(window.mermaid);
      s.onerror = () => reject(new Error(LOAD_FAILED));
      document.head.append(s);
    });
    return mermaidReady;
  }

  // Draw Mermaid source into el in the page's colors; `drawn` runs on success.
  async function mermaidInto(el, src, drawn) {
    let id = null;
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
      id = `mmd-${++mermaidN}`;
      const { svg } = await m.render(id, src);
      el.innerHTML = svg;
      fitDiagram(el.querySelector("svg"), el);
      drawn?.(el);
    } catch (e) {
      // Only a diagram the library rejects is the author's to fix, not a library that failed to load.
      // A failed render leaves Mermaid's error graphic at the end of the page.
      if (id) for (const junk of [document.getElementById(id), document.getElementById(`d${id}`)]) junk?.remove();
      const issue = e.message === LOAD_FAILED ? null : `Mermaid: ${e.message || e}`;
      el.replaceChildren(h("pre", { class: "diagram-error", "data-issue": issue }, `The diagram failed to draw: ${e.message || e}\n\n${src}`));
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

  return {
    $, esc, h, storage, debounce, clock, isDark, narrow, getJSON, post,
    toast, banner, applyTheme, nextTheme, resizable, bindResizers,
    knownPresence, showPresence, presenceNote, questionState, tickElapsed, mermaidInto,
  };
})();
