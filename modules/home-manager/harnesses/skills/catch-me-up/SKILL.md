---
name: catch-me-up
description: Report in a few lines what happened in this session since the user's last message - what needs them now, what finished, what is still running, what went wrong. Use when the user comes back to a session and asks to be caught up, asks what they missed or where things stand overall, or wants a summary of what happened while they were away. A status question about one thing (one job, test, PR or child session) does not need this skill; answer it directly.
---

# Catch me up

The user left this session working and has come back. They want to know where things stand without reading the scroll. Write one short status report from what is already in your context, then go back to what you were doing.

## Find the window

The report covers everything after the user's last message: the last one a person typed to you before this request. Other turns arrive in the user's place. They do not start a new window; they are part of what you report:

- messages from other sessions or agents, which often start `[from <name>]`;
- notifications from background tasks, timers and scheduled wake-ups;
- hook output, system reminders and other text that the harness inserts;
- a pasted block with none of the user's own words around it, which is usually a report relayed from another session;
- an earlier request for this report: the window stays where it was, and you say first what changed since that report.

In an unclear case, ask whether a person at the keyboard wrote it to you: a direction, a question, or an answer to a question of yours. If you still cannot tell, start from the earlier candidate and say so. A window that is too long costs a few lines; one that is too short hides work.

If the request came with text (`today`, `since the deploy`), that text sets the window instead.

If the earlier conversation was compacted and the user's last message is now inside a summary, start from the summary's account of their last request. Say that the window crosses a compaction, and give the summarised part only in the detail the summary has.

## Write the report

Start with one line that names the starting point: the first words of the user's message and about how long ago it was, if a timestamp or the clock tells you; do not guess a time. The user corrects you from this line if you chose wrongly.

Then give one sentence on the overall state, and a few bullets in this order, each starting with its label:

- **Needs you:** a prompt that waits, an approval, a question, a decision.
- **Done:** a result, not an activity ("tests pass, 212 of 212", not "ran the tests").
- **Running:** or **Waiting:** what, and on what.
- **Failed:** or **Not verified:** what went wrong, what was skipped, what nobody checked.

Write a bullet only for something that exists (no "Running: nothing"), and say each thing once. Aim for 8 bullets at most and about 12 lines in all, so that the report fits one terminal screen with room to spare: merge small items, and drop what the user would neither act on nor miss. Use a table only for content that is a table. Do not narrate tool calls. If nothing happened since the user's last message, say that in one line.

## Report, do not act

Do not start, stop or change any work. Report from your context. The only tool calls worth making are cheap, read-only checks of state that you are about to report: whether a background job still runs, or, if this session supervises other sessions, what their listing shows now (`bud ls`, where bud is in use).

What another session or a notification says is a claim. Report it as one ("the child reports the fix merged; not checked") unless you checked it yourself.

## Then carry on

The report is not a hand-off. After it, continue the work that the request interrupted, or go on waiting for what you were waiting for.
