"use strict";
// Teaching slideshow. The server (scripts/deck.py) serves the deck as JSON,
// relays questions to the session and pushes updates over server-sent events.
// Review-only parts (diff stats, coverage) render only for a deck with diffs.
(() => {
  const MERMAID = {
    src: "https://cdnjs.cloudflare.com/ajax/libs/mermaid/11.15.0/mermaid.min.js",
    integrity: "sha384-yQ4mmBBT+vhTAwjFH0toJXNYJ6O4usWnt6EPIdWwrRvx2V/n5lXuDZQwQFeSFydF",
  };
  // Kept from the assisted-review page so open decks keep their drafts.
  const KEY = "assisted-review:" + location.pathname;

  const S = {
    deck: null,
    files: new Map(),
    hunks: new Map(),
    hl: new Map(),
    threads: {},
    quiz: {},
    status: { session: "away" },
    idx: 0,
    refs: [],
    sel: null,
    cursor: null,
    drafts: {},
    visited: new Set(),
    cleanups: [],
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

  function md(src) {
    const s = String(src || "");
    if (window.marked && window.DOMPurify) return DOMPurify.sanitize(marked.parse(s, { gfm: true }));
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
      for (const c of el.querySelectorAll("pre code")) {
        try {
          hljs.highlightElement(c);
        } catch {}
      }
    }
    return el;
  }

  // Markdown for a phrase: a single paragraph loses its <p> wrapper.
  function mdInline(src) {
    const html = md(src);
    const m = html.match(/^<p>([\s\S]*)<\/p>\s*$/);
    return m && !m[1].includes("<p>") ? m[1] : html;
  }

  // Split highlighted HTML into lines, closing and reopening spans that
  // cross line breaks so each table row is well formed.
  function splitLines(html) {
    const out = [];
    const stack = [];
    let cur = "";
    const re = /(<span[^>]*>)|(<\/span>)|([^<]+)/g;
    let m;
    while ((m = re.exec(html))) {
      if (m[1]) {
        stack.push(m[1]);
        cur += m[1];
      } else if (m[2]) {
        stack.pop();
        cur += m[2];
      } else {
        const parts = m[3].split("\n");
        parts.forEach((p, i) => {
          if (i > 0) {
            out.push(cur + "</span>".repeat(stack.length));
            cur = stack.join("");
          }
          cur += p;
        });
      }
    }
    out.push(cur);
    return out;
  }

  // Highlight a hunk's old and new sides separately, so multi-line strings
  // and comments keep their colors, then pick each row from its side.
  function rowHtml(hunk) {
    if (S.hl.has(hunk.id)) return S.hl.get(hunk.id);
    const rows = hunk.rows;
    const out = rows.map((r) => esc(r[3]));
    const lang = hunk.file.lang;
    if (window.hljs && lang && hljs.getLanguage(lang)) {
      for (const side of ["-", "+"]) {
        const idx = rows.map((r, i) => i).filter((i) => (side === "-" ? rows[i][0] !== "+" : rows[i][0] !== "-"));
        if (!idx.length) continue;
        try {
          const html = hljs.highlight(idx.map((i) => rows[i][3]).join("\n"), { language: lang, ignoreIllegals: true }).value;
          const lines = splitLines(html);
          idx.forEach((rowI, k) => {
            if (side === "+" || rows[rowI][0] === "-") out[rowI] = lines[k] ?? out[rowI];
          });
        } catch {}
      }
    }
    S.hl.set(hunk.id, out);
    return out;
  }

  function rowSpan(rows, lo, hi) {
    const pick = (k) => rows.slice(lo, hi + 1).map((r) => r[k]).filter((n) => n != null);
    let [side, nums] = ["new", pick(2)];
    if (!nums.length) [side, nums] = ["old", pick(1)];
    if (!nums.length) return "?";
    return nums[0] === nums.at(-1) ? `${side} ${nums[0]}` : `${side} ${nums[0]}–${nums.at(-1)}`;
  }

  const base = (p) => p.split("/").pop();
  const lineLabel = (hk, lo, hi) => {
    const s = rowSpan(hk.rows, lo, hi);
    return `${base(hk.file.path)} ${hk.file.plain ? s.replace(/^new /, "") : s}`;
  };
  // Who answers: the deck's author, a review session when the deck has diffs.
  const who = () => (S.deck?.coverage ? "review session" : "session");

  // ---- deck state ----

  const slide = () => S.deck.slides[S.idx];
  const slideIndex = (id) => (S.deck ? S.deck.slides.findIndex((s) => s.id === id) : -1);
  const messages = (id) => S.threads[id]?.messages || [];
  const pending = (m) => m.role === "user" && (m.state === "queued" || m.state === "claimed");

  async function loadDeck() {
    const deck = await getJSON("api/deck");
    S.deck = deck;
    S.files = new Map(deck.files.map((f) => [f.key, f]));
    S.hunks = new Map();
    for (const f of deck.files) for (const hk of f.hunks) S.hunks.set(hk.id, { ...hk, file: f });
    S.hl.clear();
    if (deck.status) S.status = deck.status;
    document.title = deck.title;
    $("#deck-title").textContent = deck.title;
    const about = [deck.subtitle, deck.scope].filter(Boolean);
    $("#deck-title").title = deck.title;
    $("#deck-scope").textContent = about.join(" · ");
    $("#deck-scope").title = about.join("\n");
    $("#toc-about").replaceChildren(...about.map((t) => h("p", {}, t)), deck.goal ? h("p", {}, h("strong", {}, "Goal: "), deck.goal) : "");
  }

  async function loadThreads({ notify = false } = {}) {
    const before = new Set(Object.values(S.threads).flatMap((t) => t.messages.map((m) => m.id)));
    S.threads = await getJSON("api/threads");
    if (notify) {
      for (const [id, t] of Object.entries(S.threads)) {
        const fresh = t.messages.filter((m) => m.role === "assistant" && !before.has(m.id));
        const i = slideIndex(id);
        if (fresh.length && i >= 0 && i !== S.idx) {
          toast(`Answer on slide ${i + 1}: ${S.deck.slides[i].title}`, { label: "Go", run: () => go(i) });
        }
      }
    }
  }

  // The reader's answers on check slides come from the server so that the
  // session sees them too; the local copy covers an unreachable server.
  async function loadQuiz() {
    try {
      S.quiz = await getJSON("api/quiz");
      store.set("quiz", S.quiz);
    } catch {
      S.quiz = store.get("quiz", {});
    }
  }

  // ---- rendering ----

  function render() {
    for (const fn of S.cleanups.splice(0)) {
      try {
        fn();
      } catch {}
    }
    renderTrack();
    renderSlide();
    renderThread();
    renderNav();
    renderToc();
  }

  function renderTrack() {
    const track = $("#track");
    track.replaceChildren();
    let group = null;
    let label = null;
    S.deck.slides.forEach((s, i) => {
      if (!group || s.section !== label || !s.section) {
        group = h("div", { class: "track-sec", title: s.section || "" });
        group.style.setProperty("--n", 0);
        track.append(group);
        label = s.section;
      }
      group.style.setProperty("--n", +group.style.getPropertyValue("--n") + 1);
      const msgs = messages(s.id);
      const cls = [
        i === S.idx && "current",
        S.visited.has(s.id) && "visited",
        msgs.length && "has-thread",
        msgs.some(pending) && "pending",
        s.quiz.length && "quiz",
      ].filter(Boolean);
      group.append(
        h("button", {
          type: "button",
          class: cls.join(" "),
          title: `${i + 1}. ${s.title}${s.section ? ` (${s.section})` : ""}${s.quiz.length ? ` · check, ${scoreText(s)}` : ""}`,
          "aria-label": `Slide ${i + 1}: ${s.title}`,
          "aria-current": i === S.idx ? "step" : null,
          onclick: () => go(i),
        }),
      );
    });
  }

  function renderSlide({ keepScroll = false } = {}) {
    const s = slide();
    const el = $("#slide");
    const top = el.scrollTop;
    const inner = h(
      "div",
      { class: "slide-inner" },
      h(
        "div",
        { class: "eyebrow" },
        s.section ? h("span", { class: "sec" }, s.section) : null,
        h("span", {}, `${S.idx + 1} / ${S.deck.slides.length}`),
        s.quiz.length ? h("span", { class: "check-tag" }, `Check · ${scoreText(s)}`) : null,
        skipControls(),
      ),
      h("h2", { class: "slide-title" }, s.title),
    );
    for (const b of s.blocks) inner.append(renderBlock(b));
    if (s.quiz.length) inner.append(quizEl(s));
    if (S.idx === S.deck.slides.length - 1) inner.append(tallyEl());
    inner.append(nextUp());
    el.replaceChildren(inner);
    markAskable(el);
    paintQuoted();
    el.scrollTop = keepScroll ? top : 0;
  }

  function renderBlock(b) {
    if (b.type === "md") return h("div", { class: "block" }, mdEl(b.md));
    if (b.type === "callout") {
      return h(
        "div",
        { class: `block callout ${b.tone}`, "data-ask": b.md, "data-ask-source": "callout" },
        h("span", { class: "callout-label" }, b.tone === "risk" ? "Risk" : "Note", h("span", { class: "ask-hint" }, "Click to ask about this")),
        mdEl(b.md),
      );
    }
    if (b.type === "excerpt") return excerptEl(b);
    if (b.type === "diagram") return diagramEl(b.diagram);
    return h("div");
  }

  // The first slide of the next section, or -1. Slides without a section
  // count as sections of their own.
  function nextSection(i) {
    const slides = S.deck.slides;
    let j = i + 1;
    while (j < slides.length && slides[i].section && slides[j].section === slides[i].section) j++;
    return j < slides.length ? j : -1;
  }

  // The start of the current section, or of the previous one when already there.
  function prevSection(i) {
    const slides = S.deck.slides;
    const start = (k) => {
      while (k > 0 && slides[k].section && slides[k - 1].section === slides[k].section) k--;
      return k;
    };
    const here = start(i);
    return here < i ? here : here > 0 ? start(here - 1) : 0;
  }

  function skipControls() {
    const n = S.deck.slides.length;
    if (S.idx >= n - 1) return null;
    const next = S.deck.slides[S.idx + 1];
    const skip = nextSection(S.idx);
    return h(
      "span",
      { class: "skip" },
      h("button", { type: "button", class: "ghost small", title: `Next: ${next.title} (→)`, onclick: () => go(S.idx + 1) }, "Next →"),
      skip > S.idx + 1
        ? h(
            "button",
            { type: "button", class: "ghost small", title: `Skip the rest of ${S.deck.slides[S.idx].section} (Shift+→)`, onclick: () => go(skip) },
            `Skip to ${S.deck.slides[skip].section || S.deck.slides[skip].title} ↠`,
          )
        : skip < 0 && S.idx < n - 2
          ? h("button", { type: "button", class: "ghost small", title: "Skip to the last slide (Shift+→)", onclick: () => go(n - 1) }, "Skip to the end ↠")
          : null,
    );
  }

  function nextUp() {
    const n = S.deck.slides.length;
    if (S.idx < n - 1) {
      const nx = S.deck.slides[S.idx + 1];
      return h(
        "div",
        { class: "next-up wide" },
        h("span", {}, "Next: ", h("strong", {}, nx.title)),
        h("button", { type: "button", class: "primary", onclick: () => go(S.idx + 1) }, "Continue →"),
      );
    }
    const unseen = S.deck.slides.map((s, i) => [s, i]).filter(([s, i]) => i !== S.idx && !S.visited.has(s.id));
    return h(
      "div",
      { class: "next-up wide" },
      h(
        "span",
        {},
        unseen.length
          ? [
              "Not opened yet: ",
              ...unseen.flatMap(([s, i], k) => [
                k ? ", " : "",
                h("a", { href: `#${s.id}`, title: s.title }, String(i + 1)),
              ]),
            ]
          : "End of the walkthrough.",
      ),
      h(
        "button",
        {
          type: "button",
          class: "primary",
          onclick: async (e) => {
            e.currentTarget.disabled = true;
            try {
              await post("api/finish");
              toast(`Told the ${who()} you have finished.`);
            } catch (err) {
              toast(`Could not reach the server: ${err.message}`);
              e.currentTarget.disabled = false;
            }
          },
        },
        "Finish the walkthrough",
      ),
    );
  }

  // ---- checks ----

  // The reader's choice on question i of check slide s, or null.
  function choiceOf(s, i) {
    const c = S.quiz[s.id]?.[i];
    return Number.isInteger(c) && c >= 0 && c < s.quiz[i].choices.length ? c : null;
  }

  function score(s) {
    const picks = s.quiz.map((q, i) => choiceOf(s, i));
    const done = picks.filter((c) => c != null).length;
    const right = picks.filter((c, i) => c != null && s.quiz[i].choices[c].correct).length;
    return { total: s.quiz.length, done, right };
  }

  function scoreText(s) {
    const { total, done, right } = score(s);
    return done ? `${right} of ${total} correct` : `${total} question${total === 1 ? "" : "s"}`;
  }

  function seeLink(s, q, ch) {
    const i = slideIndex(ch.see || q.see);
    if (i < 0) return null;
    const target = S.deck.slides[i];
    return h("a", { href: `#${target.id}` }, `See slide ${i + 1}: ${target.title}`);
  }

  function quizEl(s) {
    const root = h("div", { class: "quiz" });
    s.quiz.forEach((q, i) => {
      const c = choiceOf(s, i);
      const ch = c == null ? null : q.choices[c];
      const card = h(
        "section",
        { class: `block check ${ch ? (ch.correct ? "right" : "wrong") : "open"}`, "aria-label": `Question ${i + 1}` },
        h("div", { class: "check-label" }, `Question ${i + 1} of ${s.quiz.length}`),
        mdEl(q.q, "prose check-q"),
        h(
          "ol",
          { class: "choices" },
          q.choices.map((x, j) =>
            h(
              "li",
              {},
              h(
                "button",
                {
                  type: "button",
                  class: ["choice", j === c && "chosen", ch && x.correct && "correct"].filter(Boolean).join(" "),
                  disabled: !!ch,
                  "aria-pressed": ch ? String(j === c) : null,
                  onclick: () => answer(s, i, j),
                },
                h("span", { class: "key", "aria-hidden": "true" }, String(j + 1)),
                h("span", { class: "choice-text", html: mdInline(x.text) }),
              ),
            ),
          ),
        ),
      );
      if (ch) {
        card.append(
          h(
            "div",
            { class: "check-feedback", tabindex: "-1" },
            h("div", { class: "verdict" }, ch.correct ? "Correct." : "Not quite."),
            mdEl(ch.why),
            h(
              "div",
              { class: "row" },
              ch.correct ? null : seeLink(s, q, ch),
              h("button", { type: "button", class: "ghost small", onclick: () => disagree(s, i) }, "Disagree? Ask"),
            ),
          ),
        );
      }
      root.append(card);
    });
    const { done, total } = score(s);
    if (done === total) {
      root.append(h("div", { class: "check-foot" }, h("button", { type: "button", class: "ghost small", onclick: () => retake(s) }, "Retake this check")));
    }
    return root;
  }

  async function record(s, i, choice) {
    const got = (S.quiz[s.id] ||= {});
    if (choice == null) delete got[i];
    else got[i] = choice;
    store.set("quiz", S.quiz);
    try {
      await post("api/answer", { slide: s.id, question: i, choice });
    } catch (e) {
      toast(`Could not record the answer: ${e.message}`);
    }
  }

  function answer(s, i, j) {
    if (choiceOf(s, i) != null) return;
    record(s, i, j);
    renderSlide({ keepScroll: true });
    renderTrack();
    renderToc();
    const fb = $("#slide").querySelectorAll(".check-feedback")[s.quiz.slice(0, i + 1).filter((q, k) => choiceOf(s, k) != null).length - 1];
    if (fb) {
      fb.focus({ preventScroll: true });
      fb.scrollIntoView({ block: "nearest" });
    }
  }

  function retake(s) {
    for (let i = 0; i < s.quiz.length; i++) if (choiceOf(s, i) != null) record(s, i, null);
    render();
  }

  // Quote the question, the reader's answer and the key into the question box.
  function disagree(s, i) {
    const q = s.quiz[i];
    const mine = q.choices[choiceOf(s, i)];
    const key = q.choices.find((x) => x.correct);
    addRef(quote(`Question: ${q.q}\nMy answer: ${mine.text}\nMarked correct: ${key.text}`, "quiz"));
    if (narrow()) $("#ask").classList.add("open");
    $("#question").focus();
  }

  // On the last slide: every check's outcome, with links to what was missed.
  function tallyEl() {
    const checks = S.deck.slides.map((s, i) => [s, i]).filter(([s]) => s.quiz.length);
    if (!checks.length) return null;
    let total = 0;
    let right = 0;
    const missed = [];
    const open = [];
    for (const [s, i] of checks) {
      const sc = score(s);
      total += sc.total;
      right += sc.right;
      if (sc.done < sc.total) open.push([s, i]);
      s.quiz.forEach((q, k) => {
        const c = choiceOf(s, k);
        if (c != null && !q.choices[c].correct) missed.push([s, i, q, q.choices[c]]);
      });
    }
    const item = (s, i) => h("a", { href: `#${s.id}` }, `${i + 1}. ${s.title}`);
    return h(
      "div",
      { class: "block tally" },
      h("h3", {}, `Checks: ${right} of ${total} correct`),
      missed.length
        ? h(
            "ul",
            {},
            missed.map(([s, i, q, ch]) => h("li", {}, h("span", { html: mdInline(q.q) }), " ", h("span", { class: "tally-where" }, "(", item(s, i), ") · "), seeLink(s, q, ch))),
          )
        : null,
      open.length ? h("p", {}, "Not finished: ", ...open.flatMap(([s, i], k) => [k ? ", " : "", item(s, i)])) : null,
      !missed.length && !open.length ? h("p", {}, "Nothing missed.") : null,
    );
  }

  // ---- diff excerpts ----

  // The deck's source_url template links a file's new-side lines; {end} is
  // the last line of a range and equals {line} for a single line.
  function sourceHref(path, line, end = line) {
    const tpl = S.deck.sourceUrl;
    if (!tpl) return null;
    return tpl.replaceAll("{path}", encodeURI(path)).replaceAll("{line}", line).replaceAll("{end}", end);
  }

  function sourceLink(f, b) {
    if (f.status === "deleted") return null;
    const first = b.pieces[0] && S.hunks.get(b.pieces[0].hunk);
    const row = first && first.rows.slice(b.pieces[0].lo).find((r) => r[2] != null);
    const href = sourceHref(f.path, row ? row[2] : 1);
    return href && h("a", { href, target: "_blank", rel: "noopener noreferrer" }, "View source");
  }

  // Selected lines link to the same place, when some of them exist in the
  // new file; a selection of only removed lines has nowhere to go.
  function contextLink(hk, lo, hi) {
    const nums = hk.rows.slice(lo, hi + 1).map((r) => r[2]).filter((n) => n != null);
    const href = nums.length && hk.file.status !== "deleted" ? sourceHref(hk.file.path, nums[0], nums.at(-1)) : null;
    return href && h("a", { href, target: "_blank", rel: "noopener noreferrer" }, "View in context");
  }

  function excerptEl(b) {
    const f = S.files.get(b.file);
    const head = h(
      "figcaption",
      { class: "ex-head" },
      h("span", { class: "ex-path" }, f.oldPath && f.oldPath !== f.path ? `${f.oldPath} → ${f.path}` : f.path),
      f.label ? h("span", { class: "ex-tag" }, f.label) : null,
      f.status !== "modified" && !f.plain ? h("span", { class: "ex-tag" }, f.status) : null,
      f.hunks.length && !f.plain
        ? h("span", { class: "ex-stat" }, h("span", { class: "a" }, `+${f.adds}`), " ", h("span", { class: "d" }, `−${f.dels}`))
        : null,
      sourceLink(f, b),
    );
    const fig = h("figure", { class: "excerpt wide" }, head);
    if (b.caption) fig.append(mdEl(b.caption, "ex-caption prose"));
    if (!b.pieces.length) {
      fig.append(h("div", { class: "ex-file-card" }, `${f.describe}${f.meta.length ? ` · ${f.meta.join(" · ")}` : ""}`));
      return fig;
    }
    const tbody = h("tbody");
    const table = h("table", { class: f.plain ? "diff plain" : "diff", tabindex: "0", "aria-label": `${f.plain ? "Lines" : "Diff"} of ${f.path}` }, tbody);
    for (const p of b.pieces) {
      const hk = S.hunks.get(p.hunk);
      const html = rowHtml(hk);
      if (p.note) tbody.append(h("tr", { class: "piece-note" }, h("td", { colspan: 3 }, mdEl(p.note))));
      if (!f.plain) tbody.append(h("tr", { class: "hunk" }, h("td", { colspan: 3 }, hk.header)));
      if (p.lo > 0) tbody.append(omitRow(p.lo));
      for (let i = p.lo; i <= p.hi; i++) {
        const r = hk.rows[i];
        const kind = r[0] === "+" ? "add" : r[0] === "-" ? "del" : "ctx";
        tbody.append(
          h(
            "tr",
            { class: `r ${kind}${r[4] ? " noeol" : ""}`, "data-hunk": hk.id, "data-row": i },
            f.plain ? null : h("td", { class: "ln", title: "Select to quote; Shift-click extends" }, r[1] ?? ""),
            h("td", { class: "ln", title: "Select to quote; Shift-click extends" }, r[2] ?? ""),
            h("td", { class: "code", html: f.plain ? html[i] : `<span class="sign">${r[0] === " " ? " " : r[0] === "-" ? "−" : "+"}</span>${html[i]}` }),
          ),
        );
      }
      if (p.hi < hk.rows.length - 1) tbody.append(omitRow(hk.rows.length - 1 - p.hi));
    }
    table.addEventListener("keydown", tableKeys);
    fig.append(h("div", { class: "ex-scroll" }, table));
    return fig;
  }

  const omitRow = (n) => h("tr", { class: "omit" }, h("td", { colspan: 3 }, `⋯ ${n} line${n === 1 ? "" : "s"} of this hunk not shown here`));

  // ---- line selection ----

  let drag = null;

  function rowsOf(table, hunk) {
    return [...table.querySelectorAll(`tr.r[data-hunk="${CSS.escape(hunk)}"]`)];
  }

  function setSel(table, hunk, anchor, head) {
    clearSel();
    S.sel = { table, hunk, anchor, lo: Math.min(anchor, head), hi: Math.max(anchor, head) };
    for (const tr of rowsOf(table, hunk)) {
      const i = +tr.dataset.row;
      tr.classList.toggle("sel", i >= S.sel.lo && i <= S.sel.hi);
    }
  }

  function clearSel() {
    if (!S.sel) return;
    for (const tr of S.sel.table.querySelectorAll("tr.sel")) tr.classList.remove("sel");
    S.sel.table.querySelector("tr.sel-actions")?.remove();
    S.sel = null;
  }

  function showSelActions() {
    const sel = S.sel;
    if (!sel) return;
    sel.table.querySelector("tr.sel-actions")?.remove();
    const hk = S.hunks.get(sel.hunk);
    const label = lineLabel(hk, sel.lo, sel.hi);
    const last = rowsOf(sel.table, sel.hunk).find((tr) => +tr.dataset.row === sel.hi);
    const row = h(
      "tr",
      { class: "sel-actions" },
      h(
        "td",
        { colspan: 3 },
        h(
          "div",
          { class: "row" },
          h("button", { type: "button", class: "primary small", onclick: () => quoteSel() }, `Ask about ${label}`),
          contextLink(hk, sel.lo, sel.hi),
          h("button", { type: "button", class: "ghost small", onclick: () => clearSel() }, "Cancel"),
        ),
      ),
    );
    last.after(row);
  }

  function quoteSel() {
    const sel = S.sel;
    if (!sel) return;
    const hk = S.hunks.get(sel.hunk);
    const ref = { kind: "lines", hunk: sel.hunk, lo: sel.lo, hi: sel.hi, label: lineLabel(hk, sel.lo, sel.hi) };
    clearSel();
    addRef(ref);
  }

  function addRef(ref) {
    const same = (r) => JSON.stringify(r) === JSON.stringify(ref);
    if (!S.refs.some(same)) S.refs.push(ref);
    saveDraft();
    renderChips();
    paintQuoted();
    if (narrow()) $("#ask").classList.add("open");
    $("#question").focus();
  }

  function paintQuoted() {
    for (const tr of document.querySelectorAll("#slide tr.quoted")) tr.classList.remove("quoted");
    for (const r of S.refs) {
      if (r.kind !== "lines") continue;
      for (const tr of document.querySelectorAll(`#slide tr.r[data-hunk="${CSS.escape(r.hunk)}"]`)) {
        const i = +tr.dataset.row;
        if (i >= r.lo && i <= r.hi) tr.classList.add("quoted");
      }
    }
  }

  function flashRef(r) {
    if (r.kind !== "lines") return;
    const trs = [...document.querySelectorAll(`#slide tr.r[data-hunk="${CSS.escape(r.hunk)}"]`)].filter(
      (tr) => +tr.dataset.row >= r.lo && +tr.dataset.row <= r.hi,
    );
    if (!trs.length) return;
    trs[0].scrollIntoView({ block: "center", behavior: "smooth" });
    for (const tr of trs) tr.classList.add("quoted");
    setTimeout(paintQuoted, 1600);
  }

  document.addEventListener("pointerdown", (e) => {
    const ln = e.target.closest("td.ln");
    if (!ln || e.button !== 0) return;
    const tr = ln.parentElement;
    const table = tr.closest("table.diff");
    e.preventDefault();
    const row = +tr.dataset.row;
    const hunk = tr.dataset.hunk;
    if (e.shiftKey && S.sel && S.sel.table === table && S.sel.hunk === hunk) setSel(table, hunk, S.sel.anchor, row);
    else setSel(table, hunk, row, row);
    S.cursor = { table, hunk, row };
    drag = { table, hunk };
  });
  document.addEventListener("pointermove", (e) => {
    if (!drag || !S.sel) return;
    const tr = document.elementFromPoint(e.clientX, e.clientY)?.closest("tr.r");
    if (tr && tr.closest("table.diff") === drag.table && tr.dataset.hunk === drag.hunk) {
      setSel(drag.table, drag.hunk, S.sel.anchor, +tr.dataset.row);
    }
  });
  document.addEventListener("pointerup", () => {
    if (!drag) return;
    drag = null;
    showSelActions();
  });

  function tableKeys(e) {
    const table = e.currentTarget;
    const rows = [...table.querySelectorAll("tr.r")];
    if (!rows.length) return;
    if (e.key === "ArrowDown" || e.key === "ArrowUp") {
      e.preventDefault();
      e.stopPropagation();
      const cur = S.cursor && S.cursor.table === table ? rows.findIndex((tr) => tr.dataset.hunk === S.cursor.hunk && +tr.dataset.row === S.cursor.row) : -1;
      let k = cur < 0 ? 0 : Math.max(0, Math.min(rows.length - 1, cur + (e.key === "ArrowDown" ? 1 : -1)));
      let tr = rows[k];
      if (e.shiftKey && S.sel && S.sel.table === table && tr.dataset.hunk !== S.sel.hunk) tr = rows[cur];
      for (const r of rows) r.classList.remove("cursor");
      tr.classList.add("cursor");
      tr.scrollIntoView({ block: "nearest" });
      S.cursor = { table, hunk: tr.dataset.hunk, row: +tr.dataset.row };
      if (e.shiftKey) {
        if (S.sel && S.sel.table === table && S.sel.hunk === tr.dataset.hunk) setSel(table, tr.dataset.hunk, S.sel.anchor, +tr.dataset.row);
        else setSel(table, tr.dataset.hunk, +tr.dataset.row, +tr.dataset.row);
        showSelActions();
      }
    } else if (e.key === " " && S.cursor && S.cursor.table === table) {
      e.preventDefault();
      setSel(table, S.cursor.hunk, S.cursor.row, S.cursor.row);
      showSelActions();
    } else if (e.key === "Enter" && S.sel && S.sel.table === table) {
      e.preventDefault();
      quoteSel();
    } else if (e.key === "Escape") {
      clearSel();
    }
  }

  // ---- diagrams ----

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
      markMermaid(el);
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

  function diagramEl(d) {
    const body = h("div", { class: "diagram-body" });
    const fig = h("figure", { class: "diagram wide" }, body);
    if (d.type === "svg") body.innerHTML = d.svg;
    else if (d.type === "mermaid") mermaidInto(body, d.source);
    else if (d.type === "steps") stepper(fig, body, d);
    else if (d.type === "html") {
      body.innerHTML = d.html;
      if (d.script) runWidget(body, d.script);
    }
    if (d.caption) fig.append(h("figcaption", {}, d.caption));
    return fig;
  }

  function stepper(fig, body, d) {
    const n = d.frames.length;
    let i = 0;
    let timer = null;
    const caption = h("div", { class: "frame-caption" });
    const count = h("span", { class: "count" });
    const dots = h("span", { class: "dots" }, d.frames.map(() => h("span")));
    const stop = () => {
      clearInterval(timer);
      timer = null;
      if (play) play.textContent = "Play";
    };
    const show = async (k) => {
      i = (k + n) % n;
      const fr = d.frames[i];
      if (fr.svg) body.innerHTML = fr.svg;
      else await mermaidInto(body, fr.mermaid);
      caption.textContent = fr.caption || "";
      count.textContent = `${i + 1} / ${n}`;
      [...dots.children].forEach((dot, j) => dot.classList.toggle("on", j === i));
      markAskable(body);
    };
    const play = d.autoplay
      ? h("button", {
          type: "button",
          class: "ghost small",
          onclick: () => {
            if (timer) return stop();
            play.textContent = "Pause";
            timer = setInterval(() => show(i + 1), d.autoplay);
          },
        }, "Play")
      : null;
    const controls = h(
      "div",
      { class: "stepper", tabindex: "0", "aria-label": "Diagram frames; use the arrow keys" },
      h("button", { type: "button", "aria-label": "Previous frame", onclick: () => (stop(), show(i - 1)) }, "←"),
      count,
      dots,
      h("button", { type: "button", "aria-label": "Next frame", onclick: () => (stop(), show(i + 1)) }, "→"),
      play,
    );
    controls.addEventListener("keydown", (e) => {
      if (e.key === "ArrowRight" || e.key === "ArrowLeft") {
        e.preventDefault();
        e.stopPropagation();
        stop();
        show(i + (e.key === "ArrowRight" ? 1 : -1));
      }
    });
    fig.append(caption, controls);
    S.cleanups.push(stop);
    show(0);
    if (play && !matchMedia("(prefers-reduced-motion: reduce)").matches) play.click();
  }

  function runWidget(root, script) {
    const api = {
      dark: isDark,
      ask: (text) => addRef(quote(String(text), "diagram")),
      onCleanup: (fn) => S.cleanups.push(fn),
    };
    try {
      new Function("root", "api", script)(root, api);
    } catch (e) {
      root.append(h("pre", { class: "diagram-error" }, `The interactive diagram failed: ${e.message}`));
      console.error(e);
    }
  }

  const quote = (text, source) => ({
    kind: "quote",
    source,
    text,
    label: text.replace(/[*`_]+/g, "").replace(/\s+/g, " ").trim().slice(0, 60),
  });
  const askOf = (a) => quote(a.dataset.ask, a.dataset.askSource || "diagram");

  function markAskable(el) {
    for (const a of el.querySelectorAll("[data-ask]")) {
      a.setAttribute("tabindex", "0");
      a.setAttribute("role", "button");
      if (!a.querySelector("title")) a.setAttribute("aria-label", `Ask about: ${a.dataset.ask.slice(0, 80)}`);
    }
  }

  // Mermaid draws its own SVG, so the agent cannot mark parts of it; make
  // every node, participant, message and edge label quotable by its text.
  function markMermaid(el) {
    for (const box of el.querySelectorAll("rect.actor")) {
      const g = box.parentElement;
      const label = g?.querySelector("text.actor");
      if (g && label) g.dataset.ask = label.textContent.replace(/\s+/g, " ").trim();
    }
    const parts = el.querySelectorAll("g.node, g.cluster, text.actor, text.messageText, g.edgeLabel, g.stateGroup, text.noteText");
    for (const part of parts) {
      const text = part.textContent.replace(/\s+/g, " ").trim();
      if (text && !part.closest("[data-ask]")) part.dataset.ask = text;
    }
    markAskable(el);
  }

  document.addEventListener("click", (e) => {
    const a = e.target.closest("#slide [data-ask]");
    if (!a || e.target.closest("a, button, input, label") || !getSelection().isCollapsed) return;
    addRef(askOf(a));
  });
  document.addEventListener("keydown", (e) => {
    const a = e.target.closest?.("#slide [data-ask]");
    if (a && e.target === a && (e.key === "Enter" || e.key === " ")) {
      e.preventDefault();
      addRef(askOf(a));
    }
  });

  // Selecting text anywhere on a slide (outside diff excerpts, which have
  // line selection) offers to quote it.
  const quoteBtn = h("button", { type: "button", class: "quote-btn primary small", hidden: true }, "Ask about this text");
  document.body.append(quoteBtn);

  function slideSelection() {
    const sel = getSelection();
    if (!sel || sel.isCollapsed || !sel.rangeCount) return null;
    const range = sel.getRangeAt(0);
    const node = range.commonAncestorContainer;
    const el = node.nodeType === 1 ? node : node.parentElement;
    if (!el || !el.closest("#slide") || el.closest("table.diff")) return null;
    const text = sel.toString().trim();
    return text.length > 1 ? { text, rect: range.getBoundingClientRect() } : null;
  }

  document.addEventListener(
    "selectionchange",
    debounce(() => {
      const s = slideSelection();
      quoteBtn.hidden = !s;
      if (!s) return;
      const top = Math.min(innerHeight - 44, s.rect.bottom + 8);
      const left = Math.max(8, Math.min(innerWidth - quoteBtn.offsetWidth - 8, s.rect.left));
      Object.assign(quoteBtn.style, { top: `${top}px`, left: `${left}px` });
    }, 120),
  );
  quoteBtn.addEventListener("mousedown", (e) => e.preventDefault());
  quoteBtn.addEventListener("click", () => {
    const s = slideSelection();
    quoteBtn.hidden = true;
    if (!s) return;
    getSelection().removeAllRanges();
    addRef(quote(s.text.slice(0, 2000), "text"));
  });

  // ---- questions ----

  function renderThread() {
    const s = slide();
    const msgs = messages(s.id);
    $("#ask-sub").textContent = `${S.idx + 1}. ${s.title}`;
    $("#clear-btn").hidden = !msgs.length || msgs.some(pending);
    const t = $("#thread");
    t.replaceChildren();
    if (S.status.session === "away") {
      t.append(
        h(
          "p",
          { class: "offline-note" },
          `The ${who()} is not listening right now. Questions wait until it is; to resume, ask it in the terminal to keep listening.`,
        ),
      );
    }
    if (!msgs.length) {
      const empty = h("div", { class: "thread-empty" }, h("p", {}, `No questions on this slide yet. The ${who()} that wrote the deck answers here.`));
      if (s.prompts.length) {
        empty.append(
          h(
            "div",
            { class: "prompt-list" },
            s.prompts.map((p) =>
              h("button", { type: "button", onclick: () => ((($("#question").value = p), updateSend(), saveDraft()), $("#question").focus()) }, p),
            ),
          ),
        );
      }
      t.append(empty);
    }
    for (const m of msgs) t.append(m.role === "user" ? questionEl(m) : answerEl(m));
    t.scrollTop = t.scrollHeight;
    renderChips();
    const n = msgs.filter((m) => m.role === "user").length;
    $("#ask-count").textContent = n ? `(${n})` : "";
  }

  function questionEl(m) {
    const state = {
      queued: [h("span", {}, `Waiting for the ${who()} · `, h("span", { "data-since": m.at })), h("button", { type: "button", class: "ghost small", onclick: () => withdraw(m) }, "Withdraw")],
      claimed: [h("span", { class: "dots" }, `The ${who()} is answering`), h("span", { "data-since": m.at })],
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
    return h("div", { class: "msg msg-a" }, h("div", { class: "who" }, `${who()[0].toUpperCase()}${who().slice(1)} · ${clock(m.at)}`), mdEl(m.text, "body prose"));
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
    if (!S.deck) return;
    S.drafts[slide().id] = { text: $("#question").value, refs: S.refs };
    store.set("drafts", S.drafts);
  }

  function loadDraft() {
    const d = S.drafts[slide().id] || { text: "", refs: [] };
    $("#question").value = d.text;
    S.refs = (d.refs || []).filter((r) => r.kind !== "lines" || S.hunks.has(r.hunk));
  }

  async function send() {
    const s = slide();
    const text = $("#question").value.trim();
    if (!text && !S.refs.length) return;
    $("#send").disabled = true;
    try {
      const { message } = await post("api/ask", { slide: s.id, text, refs: S.refs });
      (S.threads[s.id] ||= { messages: [] }).messages.push(message);
      $("#question").value = "";
      S.refs = [];
      saveDraft();
      paintQuoted();
      renderThread();
      renderTrack();
    } catch (e) {
      toast(`Could not send the question: ${e.message}`);
    } finally {
      updateSend();
    }
  }

  // ---- navigation, contents and chrome ----

  function go(i, { replace = false } = {}) {
    if (!S.deck) return;
    if (S.deck.slides[S.idx]) saveDraft();
    clearSel();
    S.idx = Math.max(0, Math.min(S.deck.slides.length - 1, i));
    const s = slide();
    S.visited.add(s.id);
    store.set("visited", [...S.visited]);
    store.set("slide", s.id);
    const url = `#${s.id}`;
    if (location.hash !== url) history[replace ? "replaceState" : "pushState"](null, "", url);
    loadDraft();
    render();
  }

  function renderNav() {
    const n = S.deck.slides.length;
    const prev = S.deck.slides[S.idx - 1];
    const next = S.deck.slides[S.idx + 1];
    $("#prev").disabled = !prev;
    $("#next").disabled = !next;
    $("#prev-label").textContent = prev ? prev.title : "Previous";
    $("#next-label").textContent = next ? next.title : "Next";
    $("#pos").textContent = `${S.idx + 1} / ${n}`;
  }

  function renderToc() {
    const d = S.deck;
    const c = d.coverage;
    $("#toc-cover").hidden = !c;
    $("#toc-cover").textContent = !c
      ? ""
      : `${d.stats.files} file${d.stats.files === 1 ? "" : "s"} · +${d.stats.adds} −${d.stats.dels} · ` +
        (c.appendix ? `${c.appendix} of ${c.changed} changed lines are only in the appendix` : `all ${c.changed} changed lines are shown in the walkthrough`);
    const list = $("#toc-list");
    list.replaceChildren();
    let label = null;
    d.slides.forEach((s, i) => {
      if (s.section && s.section !== label) list.append(h("li", { class: "toc-sec" }, s.section));
      label = s.section;
      const q = messages(s.id).filter((m) => m.role === "user").length;
      const sc = s.quiz.length ? score(s) : null;
      list.append(
        h(
          "li",
          { class: i === S.idx ? "current" : "" },
          h(
            "button",
            { type: "button", onclick: () => (closeToc(), go(i)) },
            h("span", { class: "toc-num" }, String(i + 1)),
            h("span", {}, s.title, sc ? h("span", { class: "toc-tag" }, "check") : null),
            h(
              "span",
              { class: `toc-q ${sc && sc.done ? (sc.right === sc.total ? "ok" : "warn") : ""}` },
              [sc && sc.done ? `${sc.right}/${sc.total}` : "", q ? `${q} question${q === 1 ? "" : "s"}` : ""].filter(Boolean).join(" · "),
            ),
          ),
        ),
      );
    });
  }

  function openToc() {
    $("#toc").hidden = false;
    $("#toc-btn").setAttribute("aria-expanded", "true");
    $("#toc .current button")?.focus();
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
    listening: ["Listening", () => `The ${who()} is waiting for questions.`],
    answering: ["Answering", () => `The ${who()} is answering a question; new ones wait their turn.`],
    away: ["Not listening", () => `The ${who()} is not waiting for questions; they are queued until it is.`],
  };

  function setStatus(st) {
    if (!st || !PRESENCE[st.session]) return;
    const changed = st.session !== S.status.session;
    S.status = st;
    const pill = $("#listen-pill");
    pill.dataset.on = String(st.session !== "away");
    pill.textContent = PRESENCE[st.session][0];
    pill.title = PRESENCE[st.session][1]();
    if (changed && S.deck) renderThread();
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

  async function refreshDeck() {
    const id = S.deck && slide()?.id;
    try {
      await loadDeck();
    } catch (e) {
      return banner("deck", `Could not reload the deck: ${e.message}`);
    }
    banner("deck", null);
    const i = slideIndex(id);
    S.idx = i >= 0 ? i : Math.min(S.idx, S.deck.slides.length - 1);
    loadDraft();
    render();
    toast(`The ${who()} updated the deck.`);
  }

  function connect() {
    const es = new EventSource("api/events");
    let failures = 0;
    const threadsChanged = debounce(async () => {
      try {
        await loadThreads({ notify: true });
        renderThread();
        renderTrack();
        renderToc();
      } catch {}
    }, 150);
    const quizChanged = debounce(async () => {
      const before = JSON.stringify(S.quiz);
      await loadQuiz();
      if (JSON.stringify(S.quiz) === before) return;
      if (slide().quiz.length || S.idx === S.deck.slides.length - 1) renderSlide({ keepScroll: true });
      renderTrack();
      renderToc();
    }, 150);
    es.addEventListener("hello", (e) => {
      const d = JSON.parse(e.data);
      if (failures) {
        banner("conn", null);
        threadsChanged();
        quizChanged();
      }
      failures = 0;
      setStatus(d.status);
      if (S.deck && d.version !== S.deck.version) refreshDeck();
    });
    es.addEventListener("deck", () => refreshDeck());
    es.addEventListener("deck-error", (e) =>
      banner("deck", `The ${who()}'s latest edit to the deck has errors, so this page still shows the previous version.`, JSON.parse(e.data).errors),
    );
    es.addEventListener("thread", threadsChanged);
    es.addEventListener("quiz", quizChanged);
    es.addEventListener("status", (e) => setStatus(JSON.parse(e.data)));
    es.onerror = () => {
      failures += 1;
      setStatus({ session: "away" });
      if (failures > 1) banner("conn", "Lost the connection to the deck server. Reconnecting…");
    };
  }

  function bindUI() {
    $("#prev").onclick = () => go(S.idx - 1);
    $("#next").onclick = () => go(S.idx + 1);
    $("#toc-btn").onclick = () => ($("#toc").hidden ? openToc() : closeToc());
    $("#toc-close").onclick = closeToc;
    $("#deck-scope").onclick = openToc;
    $("#toc").addEventListener("click", (e) => e.target.id === "toc" && closeToc());
    $("#theme-btn").onclick = () => {
      const cur = store.get("theme", "system");
      applyTheme(THEMES[(THEMES.indexOf(cur) + 1) % THEMES.length]);
      if (S.deck) renderSlide();
    };
    $("#ask-open").onclick = () => $("#ask").classList.add("open");
    $("#ask-close").onclick = () => $("#ask").classList.remove("open");
    bindAskGrip();
    $("#clear-btn").onclick = async (e) => {
      const btn = e.currentTarget;
      if (btn.dataset.confirm !== "1") {
        btn.dataset.confirm = "1";
        btn.textContent = "Clear these questions?";
        setTimeout(() => ((btn.dataset.confirm = ""), (btn.textContent = "Clear")), 4000);
        return;
      }
      btn.dataset.confirm = "";
      btn.textContent = "Clear";
      try {
        await post("api/clear", { slide: slide().id });
        delete S.threads[slide().id];
        renderThread();
        renderTrack();
      } catch (err) {
        toast(err.message);
      }
    };
    $("#slide").addEventListener("scroll", () => (quoteBtn.hidden = true));
    const q = $("#question");
    q.addEventListener("input", () => (updateSend(), saveDraft()));
    q.addEventListener("keydown", (e) => {
      if (e.key === "Enter" && !e.shiftKey && !e.isComposing) {
        e.preventDefault();
        send();
      }
    });
    $("#composer").addEventListener("submit", (e) => (e.preventDefault(), send()));
    window.addEventListener("hashchange", () => {
      const i = slideIndex(decodeURIComponent(location.hash.slice(1)));
      if (i >= 0 && i !== S.idx) go(i, { replace: true });
    });
    matchMedia("(prefers-color-scheme: dark)").addEventListener("change", () => S.deck && renderSlide());
    document.addEventListener("keydown", (e) => {
      if (e.defaultPrevented || e.altKey || e.ctrlKey || e.metaKey) return;
      if (e.key === "Escape") {
        if (!$("#toc").hidden) return closeToc();
        if (S.sel) return clearSel();
        $("#ask").classList.remove("open");
        return;
      }
      if (e.target.closest?.("textarea, input, [contenteditable], table.diff, .stepper, .diagram-body")) return;
      const s = slide();
      if (s.quiz.length && /^[1-9]$/.test(e.key) && !e.shiftKey) {
        const i = s.quiz.findIndex((q, k) => choiceOf(s, k) == null);
        if (i >= 0 && +e.key <= s.quiz[i].choices.length) {
          e.preventDefault();
          answer(s, i, +e.key - 1);
        }
        return;
      }
      if (s.quiz.length && e.key === "Enter" && !e.target.closest?.("button, a")) {
        const { done, total } = score(s);
        if (done === total && S.idx < S.deck.slides.length - 1) {
          e.preventDefault();
          go(S.idx + 1);
        }
        return;
      }
      if (e.shiftKey && e.key === "ArrowRight") {
        e.preventDefault();
        const j = nextSection(S.idx);
        go(j >= 0 ? j : S.deck.slides.length - 1);
      } else if (e.shiftKey && e.key === "ArrowLeft") {
        e.preventDefault();
        go(prevSection(S.idx));
      } else if (e.key === "ArrowRight" || e.key === "PageDown") {
        e.preventDefault();
        go(S.idx + 1);
      } else if (e.key === "ArrowLeft" || e.key === "PageUp") {
        e.preventDefault();
        go(S.idx - 1);
      } else if (e.key === "/") {
        e.preventDefault();
        if (narrow()) $("#ask").classList.add("open");
        $("#question").focus();
      }
    });
  }

  async function boot() {
    applyTheme(store.get("theme", "system"));
    setAskWidth(store.get("ask-width", null), { save: false });
    S.drafts = store.get("drafts", {});
    S.visited = new Set(store.get("visited", []));
    bindUI();
    try {
      await loadDeck();
      await loadThreads();
      await loadQuiz();
    } catch (e) {
      $("#deck-title").textContent = "The deck server is not reachable";
      banner("deck", `Could not load the deck: ${e.message}. Ask the session to start the server again.`);
      return;
    }
    const fromHash = slideIndex(decodeURIComponent(location.hash.slice(1)));
    go(fromHash >= 0 ? fromHash : Math.max(0, slideIndex(store.get("slide", ""))), { replace: true });
    setStatus(S.status);
    connect();
    setInterval(tickElapsed, 1000);
  }

  boot();
})();
