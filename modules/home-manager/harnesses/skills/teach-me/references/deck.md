# Deck format

A deck directory holds `deck.json` and any files it quotes from there. `deck.py check DECK` validates the deck. A running server reloads the deck whenever `deck.json` or a quoted file changes. If an edit fails validation, the page keeps showing the previous version and lists the errors.

## deck.json

```json
{
  "title": "How the scheduler picks a task",
  "subtitle": "Run queues, virtual runtime and preemption",
  "scope": "kernel/sched/fair.c at v6.18",
  "goal": "Predict which task runs next from the run queue's state, and say when a waking task preempts the current one",
  "root": "/home/me/src/linux",
  "source_url": "https://github.com/torvalds/linux/blob/v6.18/{path}#L{line}-L{end}",
  "slides": []
}
```

- `title` (required): a short name for the topic. It is also the browser tab's title.
- `subtitle`, `scope`: one line each, shown under the title. Name the sources and their versions in `scope`.
- `goal` (required unless the deck has `diffs`): what the reader should be able to do afterwards. The contents panel shows it, and `check` prints it.
- `root` (optional): the directory that relative excerpt paths resolve against, absolute or relative to the deck directory; defaults to the deck directory. With `root` set, quote files saved in the deck directory by absolute path.
- `source_url` (optional): a link template to a file, with `{path}`, `{line}` and `{end}` placeholders, where `{path}` is the excerpt's path as written; excerpts quoted by absolute path get no link. Each excerpt links to its first line, and selected lines get a "View in context" link to their range; `{end}` is the range's last line and equals `{line}` for a single line. Set it when the quoted files are on GitHub at a fixed commit or tag.
- `diffs`, `appendix` (optional): diff files and appendix groups for walking through a change. A deck that declares `diffs` must show every changed line; the assisted-review skill describes them in its `references/diffs.md`.

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

- `id`: letters, digits and `. _ ~ -`, starting with a letter. It appears in the page URL, and the question box's history is stored under it, so keep it stable when you revise the deck. Ids starting with `appendix-` are reserved.
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

`check` warns about delimiters without a match. It does not validate the TeX; a formula KaTeX cannot parse shows its source in red. A reader who clicks or selects a formula quotes its TeX.

In a table cell, write `|` inside math as `\vert`; a bare `|` ends the cell.

## Checks

A check slide asks the reader a few multiple-choice questions about the slides before it. It has `quiz` in place of `blocks`:

```json
{
  "id": "check-retries",
  "title": "Check: retries",
  "section": "Key ideas",
  "quiz": [
    {
      "q": "A request has failed three times. What happens on the fourth failure?",
      "see": "retry-loop",
      "choices": [
        {"text": "It retries after 30 s", "why": "The cap applies to the delay, not the count; the fourth failure is past `max_attempts`.", "see": "retry-budget"},
        {"text": "It raises `RetryExhausted` to the caller", "correct": true, "why": "`attempts` reaches `max_attempts` (3) and the loop re-raises."},
        {"text": "It is queued for the next poll", "why": "Nothing re-enqueues failed requests; compare the loop's exit path."}
      ]
    }
  ]
}
```

- `q`: the question, in Markdown. One to six per slide, usually two or three.
- `see`: the id of the content slide that settles the question. A wrong answer links there; a choice's own `see` overrides it. It must name a slide with `blocks`, not another check.
- `choices`: two to five, each with `text` (a phrase, Markdown), `why` (a sentence of feedback shown after choosing it) and, on exactly one, `"correct": true`.

The page records the reader's answers in `DECK/quiz.json`, the last slide shows a tally with links back to what was missed, and `listen` reports the results when the reader finishes. The reader can retake a check and can quote a question they disagree with into the question box.

Write questions that are answered by the code, not by the slide's wording: predict an output for an input, name the line that enforces an invariant, say what the error path does. Make each wrong choice a plausible misreading and let its `why` say what was misread. Avoid "all of the above", trick questions and questions answerable by matching the slide's own phrases.

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

Use this for clean, precise figures. Give the SVG a `viewBox`, and color it with these classes rather than literal colors. The page styles them for light and dark themes:

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
