# Deck format

A deck directory holds `deck.json` and the diff files it names. `review_deck.py check DECK` validates the deck and its coverage. A running server reloads the deck whenever these files change. If an edit fails validation, the page keeps showing the previous version and lists the errors.

## deck.json

```json
{
  "title": "Incremental hunk parser",
  "subtitle": "Parse diffs as they stream instead of after the download",
  "scope": "Branch stream-parse against main, merge base 1a2b3c4",
  "source_url": "https://github.com/OWNER/REPO/blob/HEAD_SHA/{path}#L{line}",
  "diffs": ["change.diff"],
  "slides": [],
  "appendix": []
}
```

- `title` (required): a short name for the change. It is also the browser tab's title.
- `subtitle`, `scope`: one line each, shown under the title. State the comparison base in `scope`.
- `source_url` (optional): a link template for each excerpt's file, with `{path}` and `{line}` placeholders.
- `diffs`: the diff files, relative to the deck directory; defaults to `["change.diff"]`. Give each separate local diff a label, as in `[{"file": "staged.diff", "label": "staged"}, {"file": "unstaged.diff", "label": "unstaged"}]`. Paths from a labeled diff are written `LABEL:PATH`.

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
- `blocks`: the slide's content, rendered in order. Each block is one of:

| Block | Renders |
| --- | --- |
| `{"md": "…"}` | GitHub-flavored Markdown: prose, lists, tables, and fenced code (give pseudocode a language) |
| `{"callout": "…", "tone": "note"}` | A highlighted aside the reader can click to quote whole. Use `"tone": "risk"` for risks, tradeoffs and concerns, one per callout |
| `{"diff": "PATH", "range": "new:40-58", "caption": "…"}` | A diff excerpt with line numbers that the reader can select |
| `{"diagram": {…}}` | A diagram (see below) |

Markdown headings inside a slide start at `###`; the slide title is the larger heading.

## Diff excerpts

- `diff`: a file path as `hunks` prints it, or a hunk id (`PATH#N`).
- `range` (optional): `new:A-B` or `old:A-B` line numbers in the new or old version of the file, or `rows:A-B` (hunk ids only), using the row numbers from `hunks --show`. Without a range the excerpt shows the whole file or hunk. A file range covers every hunk that overlaps it. A `new:` range that starts at a replacement also includes the removed lines just before it; an `old:` range likewise includes the added lines just after it.
- `caption` (optional): Markdown shown above the code.
- For a file with no changed lines (a binary file, a pure rename or a mode change), `{"diff": "PATH"}` shows a summary card.

## Appendix

```json
"appendix": [
  {
    "title": "Formatter churn",
    "note": "The formatter rewrapped these files after the rename. Nothing else changed.",
    "diffs": [{"diff": "src/a.py"}, {"diff": "src/b.py", "range": "new:10-30"}]
  }
]
```

The page lays the groups out as slides after the authored ones, keeping each group together. Write the note for a reader deciding whether to look closely: say what kind of change the group is, why it was needed, and anything that makes it less routine than it looks.

## Coverage

Every changed line must appear in at least one excerpt, on a slide or in the appendix. `check` lists each missing span with a block you can paste into a slide or appendix group, for example `{"diff": "src/a.py#2", "range": "rows:5-17"}`.

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

- **Diff lines:** clicking line numbers in an excerpt (shift-click or drag to extend).
- **Callouts:** clicking a callout quotes its whole text.
- **Text:** selecting any text on a slide shows an "Ask about this text" button.
- **Mermaid diagrams:** every node, participant, message and edge label is clickable automatically.
- **`svg`, `steps` and `html` diagrams:** add `data-ask="…"` to an element to make it clickable; a click quotes that text. In `html` widgets, `api.ask(text)` quotes computed text, such as the widget's current state.

The quote carries only that text, so write `data-ask` labels that make sense out of context ("the retry queue", not "box 3"). Put separate risks, tradeoffs and open questions in separate callouts so each can be quoted on its own.
