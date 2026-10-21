# Diffs in a deck

A review deck is a `teach-me` deck (see `<skill-dir>/../teach-me/references/deck.md`) that also declares the change's diffs. `review_deck.py check DECK` then also checks coverage.

## deck.json

```json
{
  "title": "Incremental hunk parser",
  "subtitle": "Parse diffs as they stream instead of after the download",
  "scope": "Branch stream-parse against main, merge base 1a2b3c4",
  "source_url": "https://github.com/OWNER/REPO/blob/HEAD_SHA/{path}#L{line}-L{end}",
  "diffs": ["change.diff"],
  "slides": [],
  "appendix": []
}
```

- `title`: a short name for the change. State the comparison base in `scope`. `goal` is optional; the change bounds the deck.
- `source_url`: set it when the change is on GitHub (a commit, a pushed branch or a pull request), pointing at the head commit as in the example, so links open the new version of the file; leave it out for local changes.
- `diffs`: the diff files, relative to the deck directory; defaults to `["change.diff"]` when that file exists. Give each separate local diff a label, as in `[{"file": "staged.diff", "label": "staged"}, {"file": "unstaged.diff", "label": "unstaged"}]`. Paths from a labeled diff are written `LABEL:PATH`.

## Diff excerpts

`{"diff": "PATH", "range": "new:40-58", "caption": "…"}` is a slide block, alongside the base blocks.

- `diff`: a file path as `hunks` prints it, or a hunk id (`PATH#N`).
- `range` (optional): `new:A-B` or `old:A-B` line numbers in the new or old version of the file, or `rows:A-B` (hunk ids only), using the row numbers from `hunks --show`. Without a range the excerpt shows the whole file or hunk. A file range covers every hunk that overlaps it. A `new:` range that starts at a replacement also includes the removed lines just before it; an `old:` range likewise includes the added lines just after it.
- `caption` (optional): Markdown shown above the code.
- For a file with no changed lines (a binary file, a pure rename or a mode change), `{"diff": "PATH"}` shows a summary card.

Unchanged code the reader needs for context can be quoted with the base `excerpt` block; set `root` to the repository at the head commit.

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

Every changed line must appear in at least one diff excerpt, on a slide or in the appendix. `check` lists each missing span with a block you can paste into a slide or appendix group, for example `{"diff": "src/a.py#2", "range": "rows:5-17"}`.
