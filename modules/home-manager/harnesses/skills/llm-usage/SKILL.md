---
name: llm-usage
description: Answer questions about LLM subscription usage and limits across accounts (Claude, ChatGPT/Codex, Google AI), and choose the harness and model for a new agent session from current usage, with the `llm-usage` command. Use when asked how much quota is left, which account runs out first, when a limit resets, whether a long job fits on an account or harness, or where to launch a session; also before launching a substantial session when usage could matter. Not for API billing or token counts of the current conversation.
---

# llm-usage

`llm-usage` reports quota usage for the user's LLM subscriptions: every account on every host that it monitors, each limit window, and a projection to the window's reset. It reports facts and makes no choice. You choose, because you know the task. Everything about the accounts (names, plans, limits) comes from the command at run time. Do not assume it from memory or from this skill.

If the command is missing, say that usage monitoring is not set up on this host. If it cannot reach its server, say so and offer `llm-usage --local`, which reads only this user's own credentials on this host.

## Commands

Use `--json` and read the fields; the text output is for people. Add `--timeout 5` to fail fast.

| Question | Command |
| --- | --- |
| Where a new session could run here, with the facts to choose by | `llm-usage options --here --json` (optionally `--harness H`) |
| Usage, limits, projections and resets per account, on every host | `llm-usage status --json` |
| Collection health per host | `llm-usage hosts --json` |

`--here` keeps only the accounts that a harness launched from this shell would use: logins on this host in the credential directory your environment selects (for example `$CODEX_HOME`). `usable_here: null` means the server is too old to tell; treat such an entry as unverified.

## Reading `options --json`

A list with one entry per place a session could run:

- `harness` (`claude`, `codex`, `agy`), `provider`, `account`, `plan`, `usable_here`, `age_seconds`.
- `pool` and `models`, for Google only. A Google account has one entry per quota pool (for example Gemini, and other vendors' models served through Antigravity). `models` lists the `agy --model` IDs that the backend names for the pool; other models of the same vendor use the same pool.
- `flags`: facts, not verdicts. `limited` means a window is exhausted. `runs_out_early` means that at the average rate so far, a window runs out before it resets. `session_nearly_full` means a 5h session is at least 90% used. `recent_burst` means the recent rate would run out sooner than the average rate. `stale` means the data is old.
- `windows` limit every model of the entry. `model_windows` limit only the model that their `scope` names (for example `weekly (Fable)`) and have their own `flags`.
- Per window: `label`, `used_percent`, `remaining_percent`, `projected_percent` (at reset, at the average rate; null early in a window), `unused_at_reset_percent`, `resets_at_iso`, `seconds_to_reset`, `exhausts_at_iso` and `seconds_to_exhaustion` (only when it runs out before reset), `reset_since_observed`.

`status --json` holds the same windows per account, under `accounts[].windows[]`, with a one-line `summary` and a `status` per account and window: `limited` › `over` (on pace to run out early) › `under` (long window projected to end well below the limit, which is capacity that will go unused) › `ok`.

## Choosing where a session runs

Weigh the facts against the task, then launch with explicit flags, for example `bud launch <name> --harness H --model M ...`. Say in one line why you chose it when the choice is not the usual default.

- Keep the usual harness and model when nothing is flagged, or when a flag does not matter for this task: a short task on a window that runs out in 10 hours is fine.
- Move work away from an option that is `limited`, or that would run out before the task ends. Estimate the task's length; `seconds_to_exhaustion` and `seconds_to_reset` are in seconds.
- When there is a choice, prefer capacity that would otherwise go unused (high `unused_at_reset_percent`), and keep heavily used windows for work that needs them.
- If the task needs a particular model, check that model's `model_windows` too, not only `windows`.
- A Google pool has one window per pool. Read its length from the window's `label` and `seconds_to_reset`; when it is a short window (5h), it suits short or overflow work and refills soon after use.
- Switching harness changes the agent's tools and model family. Do not switch for a small gain when the task depends on a particular harness or model; tell the user the trade-off instead.
- An explicit choice by the user always wins. Report a concern; do not override the choice.

## Answering questions

- Lead with the answer, then the numbers behind it: percent used, projection, and reset time in the user's local time and as a duration ("resets Fri 01:00, in 2d13h").
- "How much is left": `remaining_percent` per account for the window asked about. If no window is named, use the long window (weekly) and mention the 5h session when it limits work.
- "Which runs out first": compare `exhausts_at` across windows. If none is set, nothing is on pace to run out, and the next reset is the nearest `resets_at`.
- "Can I start a long job on X": give the room left in both windows and how fast each is being used. Quotas are percentages, not token counts, so do not promise that a job fits.
- Say when data is `stale`, and treat a window with `reset_since_observed` as reset.
- Do not post account labels or plan details anywhere public, such as a pull request, an issue or a public repository.
