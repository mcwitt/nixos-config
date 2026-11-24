---
name: collab-edit
description: Edit one document together with the user, in turns, in their own editor - the user writes requests as comments next to passages and commits, and you answer each with an edit or a reply and commit. Use when the user asks to edit, review, polish or work through a document (a design doc, README, post, notes, a paper) with them, or asks for review comments only ("do not change my prose"). Code changes belong to a code review, not to this skill.
---

# collab-edit

You and the user edit one document together. You take turns, and every turn is a commit. The user writes in their own editor and asks for changes with comments next to the passages; you answer each comment with an edit or a reply, and commit.

The command `collab-edit` runs the session. Every session runs in a fresh Git repository of its own, apart from the document's own repository, so the back-and-forth stays out of the history that is kept. At the end, the result is merged into the original, and in a repository it becomes one commit.

## Start

```sh
collab-edit start [--comment-only] <file>
```

It prints the path of the session's copy of the document, then the session's directory. From now on, read and write only the copy. The session's directory also holds a copy of the document's figure directory (the directory beside it with the document's name, without the extension) and links to everything else in the document's directory, so figures and links to sibling files resolve. A link that climbs higher (`../../`) does not resolve during the session.

Use `--comment-only` when the user asks for review comments only, or says in other words that the prose must stay theirs (see *Comment-only mode*).

Then run `collab-edit open <session>` and tell the user what it printed. The first line is the copy's path. The second says what was done: the copy is in a terminal Emacs frame, in a background window of this tmux session, or it was not opened and why. The user edits and commits there; Magit and plain `git` work in the session's directory as in any repository.

`collab-edit status` lists the open sessions, and `collab-edit status <session>` shows one, with its mode. `<session>` is the session's directory or the original file. Check the status after a compacted or resumed conversation.

## Turns

- Commit your own edits in the session's repository, with a message that says what you did for each comment.
- Do not write to the copy while it holds changes that are not yours and are not committed.
- Remember the last commit that you saw. At the start of each of your turns, read every later commit: `git -C <session> log -p <last>..HEAD`. Each one that you did not make is the user's.

After a commit that you did not make, a message that starts `collab-edit:` arrives in this session; it names the commit, the session's directory and the subject. The message can come late or be lost (when someone types in this session, or a prompt is open), so still read the log at the start of every turn.

## Comments

A comment is a line in the format's own comment syntax: `# ...` in Org, `<!-- ... -->` in Markdown, `% ...` in LaTeX. A comment line inside a source or example block is part of the block.

A comment is about the line below it; one line holds one sentence. For several lines, the writer closes the span with a comment line `end` below the last one (`# end` in Org). For part of a sentence, the writer quotes the words in the comment.

A line that starts `agent:` is yours; any other comment line is the user's. A reply goes on a new line below, in the same block, so that a block reads as a thread:

```org
# what does this mean?
# agent: it means X. Does that sound right?
```

The last line of a block says whose turn it is: after an `agent:` line, the user's.

- Act on every comment of the user's. Make the change it asks for, or, when it asks a question or a change that you disagree with, reply in its thread.
- Do not answer in the session. Say in the session only that you replied, and where.
- A thread is resolved when the user deletes the block or replies "resolved". Then remove the block and its `end` line.
- When you remove a comment, the outcome goes into the text; the text holds no discussion.
- Outside comment-only mode, you may also start a thread: a question, or a change that you suggest and do not make. Remove your own comment when the user has dealt with it.

## Comment-only mode

The mode is recorded with the session, and `collab-edit status` shows it. In this mode, change nothing but comment lines, not even a typo; put a suggested wording inside the comment. The session's `pre-commit` hook refuses a commit of yours that changes another line or another file.

## End

The session ends when the user says so. When you infer from the context that it is done, propose to end it, and wait for the user's consent. Never end a session by yourself.

```sh
collab-edit end <session> -m "<message>"
```

The message is needed when the original is in a repository. It lists the logical changes of the session, not the back-and-forth, and follows the repository's conventions for commit messages. The command:

1. refuses while a comment remains (`--force` ends anyway), while the session's repository holds changes that are not committed, or while Emacs holds unsaved changes to the copy;
2. merges a change that someone made to the original during the session; on a conflict, it stops and names the files: resolve them in the session's repository as in any merge, commit, and run `end` again;
3. writes the document and its figure directory back in place, and in a repository commits only those paths, with the message;
4. closes the tmux window and the Emacs buffer that showed the copy, and removes the session's repository.

Tell the user what it printed. A push, and anything else that the repository's rules ask after a commit, is yours to do after the end.
