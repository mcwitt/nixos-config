"use strict";
// The review layer's page, over walkthrough's deck.js: appendix groups, the
// findings drawer and Findings slide, and the Proposed actions slide. It
// works only on a deck with diffs; on any other deck it stays out of the way.
(() => {
  const D = window.Deck;
  const { S, $, h, mdEl, mdInline, chipLabel, excerptEl, renderBlock, renderSlide, keepFocus, markAskable } = D;
  const { paintQuoted, renderChips, saveDraft, flashRef, go, slide, slideIndex, sourceHref, toast, post, getJSON, who, debounce } = D;

  Object.assign(S, {
    groupsOpen: new Map(),
    editing: new Map(),
    findings: [],
    actions: [],
    review: { hidden: 0, review: "none", reveal: "hidden", proposal: "none" },
  });

  // An appendix group: a note saying whether the changes deserve a close
  // look, with a long diff folded away until the reader opens it. A group
  // alone on its slide shares the slide's title, so the stylesheet hides its own.
  const FOLD_LINES = 30;
  function groupEl(b, { where, audit }) {
    const key = `${slide().id}:${where}`;
    let adds = 0;
    let dels = 0;
    for (const ex of b.blocks) {
      for (const p of ex.pieces) {
        for (const r of S.hunks.get(p.hunk).rows.slice(p.lo, p.hi + 1)) {
          if (r[0] === "+") adds++;
          else if (r[0] === "-") dels++;
        }
      }
    }
    const files = new Set(b.blocks.map((ex) => ex.file)).size;
    const diffs = h(
      "details",
      { class: "group-diffs", open: audit || (S.groupsOpen.get(key) ?? adds + dels <= FOLD_LINES) },
      h("summary", {}, `Changes in ${files} file${files === 1 ? "" : "s"}`, adds || dels ? h("span", { class: "ex-stat" }, h("span", { class: "a" }, ` +${adds}`), " ", h("span", { class: "d" }, `−${dels}`)) : null),
      b.blocks.map((ex, k) => renderBlock(ex, { where: `${where}.blocks[${k}]`, audit })),
    );
    if (!audit) diffs.addEventListener("toggle", () => S.groupsOpen.set(key, diffs.open));
    return h("section", { class: "block group wide" }, h("h3", {}, b.title), mdEl(b.md), diffs);
  }

  // ---- findings ----

  // Review findings: the reader's, kept in a drawer from the top bar, and
  // laid out on the Findings slide at the end, where the reader decides what
  // to do with each. A background review's findings stay hidden until the
  // reader reveals them there, so they do not steer the reader's own; the
  // session then merges duplicates. Clicking a finding quotes it into the
  // question box.
  const SEVERITIES = ["P0", "P1", "P2", "P3"];
  const SEVERITY_HELP = {
    P0: "Blocks the release or major use: a security hole, data loss",
    P1: "Urgent: fix it in the next cycle",
    P2: "Fix it eventually",
    P3: "Nice to have",
  };
  const findingsOn = () => !!S.deck?.coverage;

  async function loadReview() {
    if (!findingsOn()) return;
    const { findings, actions, ...review } = await getJSON("api/review");
    S.findings = findings;
    S.actions = actions;
    S.reviewAt = Date.now() / 1000; // resend_in counts from here, by this page's clock
    S.review = review;
    registerFiles(actions.flatMap((a) => a.diff?.files || []));
  }

  async function reviewOp(op) {
    try {
      return await post("api/review", op);
    } catch (e) {
      toast(`Could not do that: ${e.message}`);
    }
  }

  function renderFindings() {
    const on = findingsOn();
    $("#findings-btn").hidden = !on;
    if (!on) return;
    $("#findings-count").textContent = S.findings.length || "";
    $("#findings-list").replaceChildren(
      ...(S.findings.length ? S.findings.map((f) => findingEl(f)) : [h("p", { class: "thread-empty" }, "No findings yet. Write one in the question box and press Add finding.")]),
    );
    markAskable($("#findings-list"));
  }

  // A finding with its severity, sources, text and places. On the Findings
  // slide it also offers the actions.
  function findingEl(f, { actions = false } = {}) {
    const own = f.sources.includes("reader");
    const severity = h(
      "select",
      { class: `severity ${f.severity || "none"}`, "aria-label": "Severity", title: SEVERITY_HELP[f.severity] || "No severity yet" },
      h("option", { value: "" }, "P?"),
      SEVERITIES.map((p) => h("option", { value: p, title: SEVERITY_HELP[p], selected: p === f.severity }, p)),
    );
    severity.addEventListener("change", () => reviewOp({ op: "set", id: f.id, severity: severity.value || null }));
    const sources = sourceTags(f.sources);
    const merged = f.merged?.length
      ? h(
          "details",
          { class: "merged" },
          h("summary", {}, `Also reported by ${[...new Set(f.merged.flatMap((m) => m.sources))].map(sourceName).join(" and ")}`),
          f.merged.map((m) => h("div", { class: "merged-item" }, sourceTags(m.sources), mdEl(m.text))),
        )
      : null;
    // The drawer and the slide show the same finding; each has its own editor.
    const editKey = `${actions ? "slide" : "drawer"}:${f.id}`;
    const text = S.editing.has(editKey)
      ? editorEl(editKey, (t) => reviewOp({ op: "set", id: f.id, text: t }))
      : h("div", { class: "finding-text", "data-ask": `Finding ${f.id} (${f.severity || "no severity"}): ${f.text}`, "data-ask-source": "finding" }, mdEl(f.text));
    const places = f.refs.length
      ? h("div", { class: "chips" }, f.refs.map((r) => h("button", { type: "button", class: "chip static", title: r.text || r.path || r.label, onclick: () => showRef(f, r) }, chipLabel(r))))
      : null;
    const tools = h(
      "div",
      { class: "finding-tools" },
      actions ? actionButtons(f) : null,
      own ? h("button", { type: "button", class: "ghost small", onclick: () => startEditing(editKey, f.text) }, "Edit") : null,
      own ? h("button", { type: "button", class: "ghost small", onclick: () => reviewOp({ op: "remove", id: f.id }) }, "Remove") : null,
    );
    return h("article", { class: `finding ${f.action || ""}`, "data-finding": f.id }, h("div", { class: "finding-head" }, severity, sources, tools), text, merged, places);
  }

  const sourceName = (s) => (s === "reader" ? "you" : "the review");
  const sourceTags = (sources) => sources.map((s) => h("span", { class: `source ${s}` }, s === "reader" ? "you" : "review"));

  // Where the background review stands, and the button that shows its findings.
  function revealEl() {
    const { hidden, review, reveal } = S.review;
    if (reveal === "shown") return null;
    if (reveal === "requested") {
      return h(
        "div",
        { class: "reveal" },
        h("p", {}, h("span", { class: "ellipsis" }, `The ${who()} is merging the review's findings with yours`)),
        h("button", { type: "button", class: "ghost small", title: `Send the request to the ${who()} again`, onclick: () => reviewOp({ op: "request-reveal" }) }, "Ask again"),
      );
    }
    if (review !== "running" && !hidden) return null;
    const status = review === "running" ? "The background review is still running." : `The background review found ${hidden} issue${hidden === 1 ? "" : "s"}.`;
    return h(
      "div",
      { class: "reveal" },
      h("p", {}, `${status} Its findings stay hidden until you have added yours, so they do not steer you.`),
      h("button", { type: "button", class: "primary small", onclick: () => reviewOp({ op: "request-reveal" }) }, "Reveal the review's findings"),
    );
  }

  function actionButtons(f) {
    const choices = [["ignore", "Ignore"], ...(S.deck.pr ? [["comment", "Comment"]] : [])];
    return h(
      "span",
      { class: "segmented", role: "group", "aria-label": "What to do" },
      choices.map(([value, label]) =>
        h("button", { type: "button", class: "small", "aria-pressed": String(f.action === value), onclick: () => reviewOp({ op: "set", id: f.id, action: f.action === value ? null : value }) }, label),
      ),
    );
  }

  function renderFindingViews() {
    keepFocus(renderFindings);
    if (slide()?.generated) renderSlide({ keepScroll: true });
  }

  // An in-place editor for a finding's or a comment's text. Its draft lives
  // in S.editing, so a redraw of the slide or the drawer keeps it.
  function editorEl(key, save, hint) {
    const box = h("textarea", { class: "answer-input", rows: 4, "data-draft": key });
    box.value = S.editing.get(key);
    box.addEventListener("input", () => S.editing.set(key, box.value));
    const close = () => (S.editing.delete(key), renderFindingViews());
    const submit = async () => (await save(box.value)) && close();
    box.addEventListener("keydown", (e) => {
      if (e.key === "Enter" && (e.ctrlKey || e.metaKey)) submit();
      else if (e.key === "Escape") {
        e.preventDefault(); // the page's own Escape would also close the drawer
        close();
      }
    });
    return h(
      "div",
      { class: "finding-edit" },
      box,
      h("div", { class: "answer-row" }, h("button", { type: "button", class: "primary small", onclick: submit }, "Save"), h("button", { type: "button", class: "ghost small", onclick: close }, "Cancel"), hint ? h("span", { class: "hint" }, hint) : null),
    );
  }

  function startEditing(key, text) {
    S.editing.set(key, text);
    renderFindingViews();
    document.querySelector(`[data-draft="${CSS.escape(key)}"]`)?.focus();
  }

  // Go to where a finding points: the slide that shows its lines, the file
  // in context, or the slide it was added on.
  function showRef(f, r) {
    closeFindings();
    if (r.kind === "lines") {
      // Lines of a proposed fix are on the Proposed actions slide.
      const inFix = S.actions.some((a) => a.diff && showsLines(a.diff.blocks, r));
      const i = inFix ? slideIndex("proposed-actions") : S.deck.slides.findIndex((s) => showsLines(s.blocks, r));
      if (i >= 0) {
        go(i);
        return setTimeout(() => flashRef(r), 50);
      }
    }
    const href = r.kind !== "quote" && r.side === "new" && sourceHref(r.path, r.line, r.end);
    if (href) return window.open(href, "_blank", "noopener");
    const i = slideIndex(f.slide);
    if (i >= 0) go(i);
  }

  const showsLines = (blocks, r) =>
    blocks.some((b) => (b.type === "group" ? showsLines(b.blocks, r) : b.type === "excerpt" && b.pieces.some((p) => p.hunk === r.hunk && p.lo <= r.hi && r.lo <= p.hi)));

  function findingsSlideEl() {
    const groups = [...SEVERITIES, null].map((p) => [p, S.findings.filter((f) => (f.severity || null) === p)]).filter(([, fs]) => fs.length);
    return h(
      "div",
      { class: "block findings-slide wide" },
      revealEl(),
      mdEl(
        S.findings.length
          ? `${S.review.reveal === "shown" ? "The findings" : "Your findings"} by severity. Choose what to do with each${S.deck.pr ? ": ignore it, or comment on the pull request" : ""}, and click one to discuss it. Write a general finding in the question box and press **Add finding**.`
          : "No findings yet. To add one, write it in the question box on any slide and press **Add finding**; quoted lines become its place.",
      ),
      groups.map(([p, fs]) => h("section", { class: "finding-group" }, h("h3", {}, p ? `${p} · ${SEVERITY_HELP[p]}` : "No severity yet"), fs.map((f) => findingEl(f, { actions: true })))),
    );
  }

  async function addFinding() {
    const raw = $("#question").value.trim();
    const m = raw.match(/^(P[0-3])\b[:\s-]*/i);
    const text = m ? raw.slice(m[0].length) : raw;
    if (!text) return toast("Write the finding first.");
    const done = await reviewOp({ op: "add", text, refs: S.refs, slide: slide().id, ...(m ? { severity: m[1].toUpperCase() } : {}) });
    if (!done) return;
    $("#question").value = "";
    S.refs = [];
    saveDraft();
    renderChips();
    paintQuoted();
    toast(m ? "Added the finding." : `Added the finding; the ${who()} will give it a severity.`);
  }

  function openFindings() {
    $("#findings").hidden = false;
    $("#findings-btn").setAttribute("aria-expanded", "true");
  }
  function closeFindings() {
    $("#findings").hidden = true;
    $("#findings-btn").setAttribute("aria-expanded", "false");
  }

  // ---- proposed actions ----

  // The session proposes comments, fixes and checks for the findings; the
  // reader rewords comments, asks for changes by clicking a draft, chooses
  // what to include, and presses Carry out. A comment posts from the account
  // of whoever wrote its current text.
  const ACTION_KINDS = { comment: "Comment", fix: "Fix", check: "Check" };
  const postsAs = (a) => (a.author === "reader" ? "posts as you" : "posts as the agent's account");

  // A fix's patch draws like the deck's diffs, so its files join the deck's.
  function registerFiles(files) {
    for (const f of files) {
      S.files.set(f.key, f);
      for (const hk of f.hunks) {
        S.hunks.set(hk.id, { ...hk, file: f });
        S.hl.delete(hk.id);
      }
    }
  }

  function actionsSlideEl() {
    const { proposal } = S.review;
    const ask = (label, cls = "primary small") => h("button", { type: "button", class: cls, onclick: () => reviewOp({ op: "request-proposal" }) }, label);
    const intro = mdEl(
      `The ${who()} drafts the review comments for findings you marked **Comment**, and any fix or check you ask for. ` +
        "Reword a comment yourself, or click a draft to ask for changes. Nothing runs until you press **Carry out**.",
    );
    if (proposal === "none") return h("div", { class: "block actions-slide wide" }, intro, ask("Propose actions"));
    const drafting = proposal === "requested";
    const status = drafting ? h("p", { class: "reveal" }, h("span", { class: "ellipsis" }, `The ${who()} is drafting the actions`), " ", ask("Ask again", "ghost small")) : null;
    if (drafting && !S.actions.length) return h("div", { class: "block actions-slide wide" }, intro, status);
    // Actions already sent go again only once the session has had time to report them: it may have lost the request.
    const fresh = S.actions.filter((a) => a.include && (a.state === "proposed" || a.state === "failed"));
    const waited = Date.now() / 1000 - S.reviewAt;
    const lost = S.actions.filter((a) => a.include && a.state === "running" && waited >= a.resend_in);
    // Redraw when the next running action may go again.
    const next = Math.min(...S.actions.filter((a) => a.state === "running" && waited < a.resend_in).map((a) => a.resend_in - waited));
    clearTimeout(S.resendTimer);
    if (Number.isFinite(next)) S.resendTimer = setTimeout(renderFindingViews, next * 1000 + 500);
    const runnable = fresh.length ? fresh : lost;
    const label = drafting ? "Carry out (once the drafts are ready)" : fresh.length ? carryLabel(fresh) : lost.length ? "Send again" : "Carry out";
    return h(
      "div",
      { class: "block actions-slide wide" },
      intro,
      status,
      S.actions.map(actionEl),
      h(
        "div",
        { class: "carry-out" },
        h("button", { type: "button", class: "primary", disabled: drafting || !runnable.length, onclick: () => carryOut(runnable) }, label),
        drafting ? null : ask("Propose more", "ghost small"),
      ),
    );
  }

  function carryLabel(actions) {
    if (!actions.length) return "Carry out";
    const parts = [];
    const comments = actions.filter((a) => a.kind === "comment");
    const mine = comments.filter((a) => a.author === "reader").length;
    if (comments.length - mine) parts.push(`${comments.length - mine} comment${comments.length - mine === 1 ? "" : "s"} as the agent's account`);
    if (mine) parts.push(`${mine} comment${mine === 1 ? "" : "s"} as you`);
    for (const kind of ["fix", "check"]) {
      const n = actions.filter((a) => a.kind === kind).length;
      if (n) parts.push(`${n} ${kind}${n === 1 ? "" : kind === "fix" ? "es" : "s"}`);
    }
    return `Carry out ${actions.length}: ${parts.join(", ")}`;
  }

  async function carryOut(actions) {
    if (await reviewOp({ op: "carry-out", ids: actions.map((a) => a.id) })) toast(`Sent ${actions.length} action${actions.length === 1 ? "" : "s"} to the ${who()}.`);
  }

  function actionEl(a) {
    const locked = a.state === "running" || a.state === "done";
    const include = h("input", { type: "checkbox", checked: a.include, disabled: locked, "aria-label": "Include this action" });
    include.addEventListener("change", () => reviewOp({ op: "action-set", id: a.id, include: include.checked }));
    const finding = S.findings.find((f) => f.id === a.finding);
    const head = h(
      "div",
      { class: "finding-head" },
      include,
      h("span", { class: `source kind-${a.kind}` }, ACTION_KINDS[a.kind]),
      a.kind === "comment" ? h("span", { class: `source ${a.author}` }, postsAs(a)) : null,
      h("strong", { class: "action-title" }, a.title),
      a.state !== "proposed" ? h("span", { class: `action-state ${a.state}` }, a.state) : null,
    );
    const ask = (text) => ({ "data-ask": `Proposed ${a.kind} ${a.id} (${a.title}): ${text}`, "data-ask-source": "action" });
    let body;
    if (a.kind === "comment") {
      const place = a.place ? h("span", { class: "chip static" }, h("span", { class: "chip-label" }, a.place.label)) : h("span", { class: "source" }, "review summary");
      const key = `action:${a.id}`;
      if (S.editing.has(key)) {
        const hint = a.author === "reader" ? null : "Once you change it, the comment posts as you.";
        body = [h("div", { class: "chips" }, place), editorEl(key, (t) => reviewOp({ op: "action-set", id: a.id, body: t }), hint)];
      } else {
        const edit = locked ? null : h("div", { class: "finding-tools" }, h("button", { type: "button", class: "ghost small", onclick: () => startEditing(key, a.body) }, "Edit"));
        body = [h("div", { class: "chips" }, place), h("div", { class: "finding-text", ...ask(a.body) }, mdEl(a.body)), edit];
      }
    } else if (a.kind === "fix") {
      body = a.diff.blocks.map((b) => excerptEl(b));
    } else {
      body = [h("pre", { class: "code-answer finding-text", ...ask(a.command) }, h("code", {}, a.command))];
    }
    return h(
      "article",
      { class: `finding action ${a.include ? "" : "ignore"}`, "data-action": a.id },
      head,
      finding ? h("div", { class: "action-for" }, `For ${finding.severity || "P?"}: `, h("span", { html: mdInline(finding.text) })) : null,
      body,
      a.result ? resultEl(a) : null,
    );
  }

  function resultEl(a) {
    const r = a.result;
    return h(
      "div",
      { class: `action-result ${a.state}` },
      h("strong", {}, a.state === "done" ? "Done" : "Failed"),
      r.note ? ` · ${r.note}` : "",
      r.link ? [" · ", h("a", { href: r.link, target: "_blank", rel: "noopener noreferrer" }, "View")] : null,
      r.output ? h("details", {}, h("summary", {}, "Output"), h("pre", { class: "code-answer" }, r.output)) : null,
    );
  }

  // ---- wiring ----

  // The drawer, its button in the top bar and the composer's Add finding button.
  function init() {
    $(".bar-actions").prepend(
      h("button", { type: "button", class: "ghost", id: "findings-btn", "aria-expanded": "false", "aria-controls": "findings", hidden: true, onclick: () => ($("#findings").hidden ? openFindings() : closeFindings()) },
        "Findings ", h("span", { class: "count", id: "findings-count" })),
    );
    const drawer = h(
      "div",
      { class: "toc", id: "findings", hidden: true, onclick: (e) => e.target.id === "findings" && closeFindings() },
      h(
        "div",
        { class: "toc-panel askable", role: "dialog", "aria-label": "Findings" },
        h("div", { class: "toc-head" }, h("h2", {}, "Findings"), h("button", { type: "button", class: "ghost small", onclick: closeFindings }, "Close")),
        h("p", { class: "toc-about" }, "Click a finding to discuss it; its severity menu changes it."),
        h("div", { class: "findings-list", id: "findings-list" }),
      ),
    );
    $("#toasts").before(drawer);
    const add = h("button", { type: "button", class: "ghost small", id: "add-finding", title: "Save the text and quotes as a review finding; start with P0 to P3 to set its severity", hidden: true, disabled: true, onclick: addFinding }, "Add finding");
    $("#send").before(add);
    $("#toc-about").after(h("p", { class: "toc-cover", id: "toc-cover", hidden: true }));
  }

  // The contents panel says how much of the change the walkthrough shows.
  function renderCoverage() {
    const d = S.deck;
    const c = d.coverage;
    $("#toc-cover").hidden = !c;
    $("#add-finding").hidden = !c;
    if (!c) return;
    $("#toc-cover").textContent =
      `${d.stats.files} file${d.stats.files === 1 ? "" : "s"} · +${d.stats.adds} −${d.stats.dels} · ` +
      (c.appendix ? `${c.appendix} of ${c.changed} changed lines are only in the appendix` : `all ${c.changed} changed lines are shown in the walkthrough`);
  }

  const reviewChanged = debounce(async () => {
    try {
      await loadReview();
      renderFindingViews();
    } catch {}
  }, 150);

  D.extend({
    init,
    load: loadReview,
    deckLoaded(deck) {
      S.agent = deck.coverage ? "review session" : "session";
      registerFiles(S.actions.flatMap((a) => a.diff?.files || []));
    },
    render: [renderFindings, renderCoverage],
    reconnect: reviewChanged,
    events: { review: reviewChanged },
    escape: () => !$("#findings").hidden && (closeFindings(), true),
    composer: () => ($("#add-finding").disabled = !$("#question").value.trim()),
    // Quoting a finding into the question box closes the drawer, so the box shows.
    quoted: (el) => el.closest("#findings") && closeFindings(),
    blocks: { group: groupEl, findings: findingsSlideEl, actions: actionsSlideEl },
  });
})();
