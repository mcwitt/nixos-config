---
name: prose-review
description: Go over a Markdown document with a human in their browser, rendered beside a chat panel where they select passages to ask about or to have changed, and make the edits in the file from the session. Use when the user asks to review, polish, edit or work through prose (docs, READMEs, posts, notes, papers) with them; code changes belong to assisted-review.
---

# Prose review

Work through a Markdown document with its reader. Serve it from this machine as a rendered page with a question panel: the reader selects a passage to ask about it or to ask for a change, you answer from the session and make the edits in the file, and the page reloads with the edited passages marked. Leave Git state alone unless the user asks you to commit.

The bundled script `scripts/prose_review.py` (Python ≥ 3.11, standard library only) checks the review, serves the page and passes questions to you.

## 1. Establish the document

Resolve the file from the user's request and the conversation; if it is materially ambiguous, ask before choosing. Read the whole document, and read enough of what it describes (code, data, referenced material) to judge whether it is accurate, not only whether it reads well. Note its conventions before touching it: heading style, line wrapping, list markers, admonitions, tone and register. Your edits should be indistinguishable from the author's own.

## 2. Set up the review

Create a review directory in a scratch location outside the repository and write `review.json`:

```json
{
  "file": "/absolute/path/to/docs/guide.md",
  "title": "Optional; defaults to the first top-level heading, then the file name",
  "subtitle": "One line under the title: the document's purpose, or the review's focus",
  "source_url": "https://github.com/OWNER/REPO/blob/HEAD_SHA/docs/guide.md#L{line}-L{end}"
}
```

- `file` (required): the Markdown file, absolute or relative to the review directory. It stays where it is; the server reads it and never writes it.
- `source_url` (optional): a permalink template to the file at a pushed commit, with `{line}` and `{end}` placeholders, so selected passages get a "View in context" link. Leave it out when the file is not on GitHub or has local changes.

Run `python3 <skill-dir>/scripts/prose_review.py check REVIEW`. It validates the review and prints the outline with line numbers, which the reader's questions also use.

## 3. Serve it

```sh
python3 <skill-dir>/scripts/prose_review.py serve REVIEW --detach
```

This prints a URL with a secret token. The server listens on 127.0.0.1. If the reader's browser runs on another machine, either pass `--host` with an address that machine can reach, or have them forward the port (`ssh -L PORT:127.0.0.1:PORT HOST`). If your harness sandboxes commands, the server needs permission to listen on a port.

Give the reader the URL and a one-sentence summary of the document and the review's focus. Tell them they can select any text to quote it, then ask about it or ask for a change; that edits you make appear in the page with a mark beside each edited passage, which opens to show what it replaced, and that the "edits" button in the header clears the marks; and that "Finish" tells you they are done. Then start listening.

## 4. Answer questions and make edits

```sh
python3 <skill-dir>/scripts/prose_review.py listen REVIEW --timeout SECONDS
```

`listen` waits for the reader's next question, prints it with the selected text and its source lines, and marks it as being answered. Investigate as needed. Then write a concise Markdown answer to a file and post it while waiting for the next question:

```sh
python3 <skill-dir>/scripts/prose_review.py listen REVIEW --answer ID --file ANSWER.md --timeout SECONDS
```

Every `listen` run prints the exact command to run next. Questions and answers pass through files in the review directory, so `listen` needs no network access.

- Use the longest timeout your harness allows for a command (the default is 100 seconds). If your harness can run a command in the background and notify you when it exits, run `listen` that way, so the user can still talk to you in between. Otherwise run it in the foreground.
- When a run ends without a question, run it again. Stop listening when the reader finishes (`listen` reports it), when the user tells you to stop, or when no page has had the document open for about 30 minutes. Then tell the user how to resume.
- Answer the question asked. Where the document describes code or behavior, check the source before answering, and say what you checked. Distinguish what the text says from what you infer the author meant.
- When the reader asks for a change, edit the file in place with your usual editing tools: the smallest edit that does what was asked, in the document's own conventions. Do not reflow, reformat or "tidy" text you were not asked to change; every changed block is marked for the reader, and noise hides the real edit. The page reloads on save. Say in the answer what you changed and where (a heading, or line numbers), and anything you deliberately left as it was.
- Propose, do not make, changes nobody asked for. If you notice an error elsewhere in the document, raise it in an answer with its location and let the reader decide.
- If `listen` hands you a question you already claimed, you lost track of it; answer it now.

The page shows the reader whether you are listening, answering or away. Questions asked while you are away wait in the queue.

## 5. Finish

When the reader finishes, summarize the edits in the conversation, grouped by section, and list what you flagged but left unchanged. Say plainly that the file has uncommitted changes, unless the user asked you to commit. Stop the server with `python3 <skill-dir>/scripts/prose_review.py stop REVIEW` (it also exits after 12 idle hours). The questions and answers remain in `REVIEW/threads.json`.

## Without a browser

If no browser can reach this machine, or the reader prefers to stay in the conversation, review there. Give a one-paragraph summary of the document and its outline, then go section by section: present one section per turn, make the edits the reader asks for in the file, and quote the edited text back. Wait for the reader before moving on, and do not treat elapsed time or a missing response as permission to continue.
