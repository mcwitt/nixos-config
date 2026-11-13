# Deck format

A deck directory holds `deck.json` and any files it quotes from there. `deck.py check DECK` validates the deck, and `deck.py text DECK` prints it as Markdown in slide order, as the page shows it (`--appendix` adds the appendix, `--key` the answers to the checks). A running server reloads the deck whenever `deck.json` or a quoted file changes. If an edit fails validation, the page keeps showing the previous version and lists the errors.

## deck.json

```json
{
  "title": "How the scheduler picks a task",
  "subtitle": "Run queues, virtual runtime and preemption",
  "scope": "kernel/sched/fair.c at v6.18",
  "goal": "Predict which task runs next from the run queue's state, and say when a waking task preempts the current one",
  "reader": "Knows: C, processes and threads, and what a scheduler is for. New to: the kernel's source and its fair-scheduling class.",
  "audience": 3,
  "detail": 3,
  "root": "/home/me/src/linux",
  "source_url": "https://github.com/torvalds/linux/blob/v6.18/{path}#L{line}-L{end}",
  "slides": []
}
```

- `title` (required): a short name for the topic. It is also the browser tab's title.
- `subtitle`, `scope`: one line each, shown under the title. Name the sources and their versions in `scope`.
- `goal` (required): what the reader should be able to do afterwards. The contents panel shows it, and `check` prints it.
- `reader` (optional): what the reader knows, then what they are new to, in two sentences. `check` prints it, and the cold read needs it (see `cold-read.md`).
- `audience`, `detail` (optional): the stops the deck is written at, 1 to 5 or a stop's name (see `dials.md`). With them, the page shows the dials on each content slide, and the slides rewritten at other settings live in `DECK/variants/`. `detail` defaults to 3 when `audience` is given.
- `root` (optional): the directory that relative excerpt paths resolve against, absolute or relative to the deck directory; defaults to the deck directory. With `root` set, quote files saved in the deck directory by absolute path.
- `source_url` (optional): a link template to a file, with `{path}`, `{line}` and `{end}` placeholders, where `{path}` is the excerpt's path as written; excerpts quoted by absolute path get no link. Each excerpt links to its first line, and selected lines get a "View in context" link to their range; `{end}` is the range's last line and equals `{line}` for a single line. Set it when the quoted files are on GitHub at a fixed commit or tag.

## Slides

```json
{
  "id": "retry-loop",
  "title": "Retries back off exponentially",
  "section": "Key ideas",
  "blocks": [],
  "prompts": ["Why cap the delay at 30 s?"]
}
```

- `id`: letters, digits and `. _ ~ -`, starting with a letter. It appears in the page URL, and the question box's history is stored under it, so keep it stable when you revise the deck.
- `section` (optional): a label such as "Why" or "Key ideas". Consecutive slides with the same label are grouped in the progress bar and the contents list.
- `prompts` (optional): suggested questions, shown while the slide's question box is empty.
- `blocks`: the slide's content, rendered in order (a check slide has `quiz` instead; see below). Each block is one of:

| Block | Renders |
| --- | --- |
| `{"md": "…"}` | GitHub-flavored Markdown: prose, lists, tables, math, and fenced code (give pseudocode a language) |
| `{"callout": "…", "tone": "note"}` | A highlighted aside the reader can click to quote whole. Use `"tone": "risk"` for risks, tradeoffs and concerns, one per callout |
| `{"excerpt": "PATH", "range": "40-58", "caption": "…"}` | Lines of a file with line numbers that the reader can select |
| `{"diagram": {…}}` | A diagram (see below) |

Markdown headings inside a slide start at `###`; the slide title is the larger heading.

## Math

Markdown fields render TeX with KaTeX: blocks, callouts, captions, check questions, choices and feedback, prompts and answers.

- Inline: `$x_1$` or `\(x_1\)`.
- Display: `$$…$$` or `\[…\]`, or a fenced block with the language `math`.
- A `$` opens math only when the next character is not a space, and closes it only when the previous character is not a space and the next is not a digit, so `$5 and $6` stays text. Write a literal dollar elsewhere as `\$`.
- `\(` and `\[` always start math. Code spans and code blocks are never math.

`check` warns about delimiters without a match. It does not validate the TeX; a formula KaTeX cannot parse shows its source in red, and `listen` reports it once a page has drawn the deck. A reader who clicks or selects a formula quotes its TeX. The question panel typesets math in quotes and in the reader's questions.

In a table cell, write `|` inside math as `\vert`; a bare `|` ends the cell.

## Checks

A check slide asks the reader a few questions about the slides before it: multiple choice, or written answers in prose or code that the session judges. It has `quiz` in place of `blocks`:

```json
{
  "id": "check-retries",
  "title": "Check: retries",
  "section": "Key ideas",
  "quiz": [
    {
      "q": "A retry loop allows `max_attempts = 3` retries, with delays capped at 30 s. A request has failed and been retried three times. What happens on the next failure?",
      "see": "retry-loop",
      "choices": [
        {"text": "It retries after 30 s", "why": "The cap applies to the delay, not the count; the next failure is past `max_attempts`.", "see": "retry-budget"},
        {"text": "It raises `RetryExhausted` to the caller", "correct": true, "why": "All three retries are used, so the loop re-raises."},
        {"text": "It is queued for the next poll", "why": "Nothing re-enqueues failed requests; compare the loop's exit path."}
      ]
    }
  ]
}
```

- `q`: the question, in Markdown, which can hold links and fenced code. One to six per slide, usually two or three.
- `see`: the id of the content slide that settles the question. A wrong answer links there; a choice's own `see` overrides it. It must name a slide with `blocks`, not another check.
- `choices`: two to five, each with `text` (a phrase, Markdown), `why` (a sentence of feedback shown after choosing it) and, on exactly one, `"correct": true`.

### Written answers

```json
{"q": "When the retry loop runs out of retries, why does it re-raise the last error instead of returning `None`?", "see": "retry-loop", "kind": "prose",
 "rubric": "Callers must see the last error; `None` would look like a successful empty result.",
 "model": "So the caller sees the final error rather than a `None` it would take for an empty result."}
```

```json
{"q": "Write `backoff(n)`: the delay before retry `n`, doubling from 1 s and capped at 30 s.", "see": "retry-loop",
 "kind": "code", "language": "python", "starter": "def backoff(n):\n    ...\n",
 "rubric": "Returns 2**(n-1) seconds, at most 30; n starts at 1.",
 "test": {"dir": "tests/backoff", "file": "answer.py", "command": "python3 -m pytest -q", "timeout": 60}}
```

- `kind`: `prose` for an answer in the reader's own words, `code` for code; `choice` (the default) for multiple choice. A written question has no `choices`.
- `rubric` (required): what a correct answer must contain. The session judges against it.
- `model` (optional): a model answer, Markdown for prose and code for code. The page offers it after judging.
- `language`, `starter` (code, optional): the highlighting language, and code the editor starts with.
- `test` (code, optional): `listen` copies `dir` (a subdirectory of the deck directory; default empty) to `DECK/runs/ID/`, writes the answer there as `file`, runs `command` in it with a time limit of `timeout` seconds (default 60), and shows the result with the answer. The web server never runs code. Write the tests before serving, and check that they pass on the model answer and fail on a plausible wrong one.

A submitted answer queues for the session like a question. `listen` hands it over with the rubric, the model answer and any test results, and the session posts feedback with a verdict: `correct`, `partial` or `incorrect`. Only `correct` counts as right in the tally. The reader can revise and resubmit.

Ask for a written answer where choosing among options would let the reader guess: explain why a design holds, predict a behavior and justify it, or write the few lines that implement an idea, given the signature and the names they may use. A written answer costs the reader minutes, not seconds, so use one or two per deck, usually in the final check.

The page records the reader's answers in `DECK/quiz.json`, a tally of every check, with links back to what was missed, follows the last check once it is answered (or, if content follows the last check, sits on the last slide before the appendix), and `listen` reports the results when the reader finishes. The reader can retake a check and can quote a question they disagree with into the question box.

Write questions that test what the material implies, not the slide's wording: predict an output for an input, say what enforces an invariant, say what the error path does. Make each wrong choice a plausible misreading and let its `why` say what was misread. Avoid "all of the above", trick questions and questions answerable by matching the slide's own phrases.

Make each question stand on its own. The reader sees neither the code nor the earlier slides, and may not remember the names. In the question, its choices and its `why`, give the context in one of these ways:

- **Spell it out.** Say what each identifier means, or describe the thing in plain words and drop the name. Quote the code that the question relies on, in inline code or a fenced block. Do this when the context is short.
- **Link to it.** Link the identifier or phrase to the code, when the code is too long to quote. With `source_url`, fill in its template by hand for each link, for example ``[`slot`](https://github.com/OWNER/REPO/blob/SHA/ring.py#L124-L134)``; the link opens in a new tab. Without `source_url`, or for a file it does not cover, link to the slide that shows the code, as `[the gather](#slide-id)`, or spell the context out. A file path alone is not a link.

Never rely on a bare line number: a question that asks for "line 71", or for which line does something, must show those lines. Otherwise ask for the step or the condition.

Give what the reader needs to reason, not the conclusion. Define a name, but do not say how it behaves in the case you ask about. Quote or link the inputs and the surrounding function, not only the line that decides the answer. Leave out details that the reasoning does not need.

Before: "A padding token has `selected = [3, 9, 12, 20]` with `local_experts = 4`. Its `combine_weights` entry at `k = 1` is nonzero. What gradient does that entry get after this PR, and which line makes it so?"

After: "`combine_weights[t, k]` is the weight that token `t` gives to the `k`-th of its selected experts. A padding token has in-range selected experts and a nonzero weight at `k = 1`. What gradient does that weight get after this PR, and which step of [the backward pass](https://github.com/OWNER/REPO/blob/SHA/ring.py#L64-L71) makes it so?"

## Excerpts

- `excerpt`: a file path, relative to `root`, or absolute. Save fetched text (a paper, a page, documentation) into the deck directory as a plain text file and quote it from there.
- `range` (optional): `A-B` or `A`, line numbers in the file. Without a range the excerpt shows the whole file, so give one for anything longer than a screen.
- `caption` (optional): Markdown shown above the lines.

Code is highlighted by its file extension. Keep excerpts short: the lines that carry the idea, not the whole function.

## Appendix and the final check

A slide with `"section": "Appendix"` belongs to the appendix: material the goal does not need, for readers who want more. Put appendix slides last. A deck with more than three content slides outside the appendix must have a check as its last slide before the appendix.

`check` prints every slide with the checks that point at it, and warns about content slides outside the appendix that no check question or choice names in `see`. The warning is advice: point a question at a slide the goal depends on, move a slide it does not depend on to the appendix, or leave a slide that needs no check.

## Diagrams

Every diagram takes an optional `caption`. `alt` is a short text description; give one for every type except `mermaid`.

### svg

```json
{"type": "svg", "svg": "<svg viewBox='0 0 420 120'>…</svg>", "caption": "…", "alt": "…"}
```

Use this for clean, precise figures. Give the SVG a `viewBox` that fits the drawing: the page shows it at 1.25 times the viewBox width, shrinking it to fit the column, so a 400-wide viewBox shows 500 pixels wide and 13-pixel text shows near the size of the prose. Size boxes for their text; the page reports text wider than its box, or outside the viewBox, to `listen`. Color the SVG with these classes rather than literal colors. The page styles them for light and dark themes:

- Boxes: `d-box`, `d-box-soft`, `d-box-accent`, `d-box-add` (new), `d-box-del` (removed), `d-box-hl` (highlighted)
- Lines: `d-edge`, `d-edge-accent`, `d-edge-dashed`; for arrowheads, a `<marker>` whose path has `d-fill-ink` or `d-fill-accent`
- Text: `<text>` uses the body font and color; add `d-mono` for code and `d-muted` for secondary labels

Give marker and gradient ids a slide-specific prefix, because all of a slide's SVGs share one document.

### mermaid

```json
{"type": "mermaid", "source": "flowchart LR\n  A --> B", "caption": "…"}
```

Use this for quick flowcharts, sequence diagrams and state machines. The page themes it to match.

### steps

```json
{"type": "steps", "autoplay": 1600, "caption": "…", "alt": "…",
 "frames": [{"svg": "<svg …>", "caption": "Before"}, {"mermaid": "…", "caption": "After"}]}
```

A frame-by-frame animation that the reader steps through with arrow buttons or keys. With `autoplay` (milliseconds per frame) it also plays on its own until the reader pauses it, unless they prefer reduced motion. Use it for an algorithm's iterations, state changing over time, or before and after. Draw the frames with the same layout so that only what changes moves.

### html

```json
{"type": "html", "html": "<div class='w'>…</div>", "script": "…", "caption": "…", "alt": "…"}
```

An interactive widget. `script` is the body of a function called with `root` (the diagram's element, containing `html`) and `api`:

- `api.dark()` returns true in the dark theme.
- `api.ask(text)` quotes `text` into the question box.
- `api.onCleanup(fn)` runs `fn` when the reader leaves the slide (clear timers and listeners there).

Use the page's CSS variables for colors: `--ink`, `--ink-2`, `--ink-3`, `--panel`, `--sunk`, `--rule`, `--accent`, `--accent-wash`, `--add-row`, `--add-ink`, `--del-row`, `--del-ink`, `--sel-row` and `--sel-edge`. For fonts use `--font-body` and `--font-mono`. Scope the widget's styles under its root class, keep it self-contained, and do not fetch anything from the network.

## Quoting into questions

Readers can quote parts of a slide into a question, and the quote reaches you with the question:

- **Lines:** clicking line numbers in an excerpt (shift-click or drag to extend). With `source_url` set, the selection also offers "View in context", which opens those lines in the file.
- **Callouts:** clicking a callout quotes its whole text.
- **Text:** selecting any text on a slide shows an "Ask about this text" button.
- **Mermaid diagrams:** every node, participant, message and edge label is clickable automatically.
- **`svg`, `steps` and `html` diagrams:** add `data-ask="…"` to an element to make it clickable; a click quotes that text. In `html` widgets, `api.ask(text)` quotes computed text, such as the widget's current state.

The quote carries only that text, so write `data-ask` labels that make sense out of context ("the retry queue", not "box 3"). Put separate risks, tradeoffs and open questions in separate callouts so each can be quoted on its own.
