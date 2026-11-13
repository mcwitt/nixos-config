# Audience and detail

A deck is written at one setting of two dials, each with five stops. **Audience** sets how much background the deck assumes and how much it supplies. **Detail** sets how much of the material the deck covers and how deeply. The dials are independent: a novice can ask for a concise deck, which is short and still defines its terms, and an expert can ask for a complete one, which covers everything and defines nothing.

Record the setting in `deck.json` as `"audience": N` and `"detail": N` (1 to 5). The page then shows both dials on every content slide, and the reader can ask for a slide, or all of them, at another setting. `deck.py` and `listen` quote the rows below; keep each row's text self-contained.

## Audience

Each stop knows everything the stops below it know, plus one more layer; each row names all of its layers. For a topic that is not code, such as a paper or a concept, read "the project" as the research area or community the topic belongs to, and "the subsystem" as the line of work the topic extends.

| Stop | Name | The reader knows | The deck supplies |
| --- | --- | --- | --- |
| 1 | novice | Programming in general and the language of the sources, nothing of the field. | A plain-words definition of every term of the field, the project and the subject where it first appears. Background for each prerequisite idea that the goal needs (at most two background slides or opening blocks). One concrete example or analogy for the central idea, then the real mechanism by its real name. An explanation of each quoted line. |
| 2 | newcomer | Programming, and the field (for example ML training, JAX, distributed systems) at a working level; not this project. | Definitions of the project's terms and of the field's specialist terms. One short orientation to the part of the project where the subject lives. No definitions of the field's common terms and no analogies for them. |
| 3 | practitioner | Programming, the field, and the project as a user: its main parts, its vocabulary and how it runs. | Definitions of the subsystem's terms and of the names the subject introduces. A sentence on each piece of unchanged code that the subject relies on. |
| 4 | specialist | Programming, the field, the project, and the code of the subsystem as one of its maintainers. | Definitions only of names that the subject introduces. The subsystem's idioms, invariants and earlier designs by name, without explanation. The words go to what is new and why. |
| 5 | expert | Programming, the field, the project, the subsystem's code, and the work around the subject: earlier changes, the discussion, the authors' names for things. Only the subject itself is new. | No definitions or background. The delta and its consequences, stated directly: the claims, the lines that carry them, and the risks. Earlier work by reference, not by summary. |

Write `reader` (see `cold-read.md`) to match the stop: "Knows" names the layers the stop includes, with the specific parts of the project; "New to" names the layers above it. The stop says how far the reader's knowledge goes; `reader` says what is in it.

Treat the reader at every stop as capable. At stops 1 and 2, define a term in one clause and move on. Do not write "simply", "just", "don't worry" or "as you may know", and use at most one analogy per slide.

## Detail

Word counts are prose and callouts on one content slide. Two things come on top of them: definitions and background that the audience stop asks for, and, for a change, what the list below the table keeps. The final check stays at every stop (one or two questions at stop 1), even when the deck has three content slides or fewer and `check` does not require it.

| Stop | Name | The deck covers | One slide holds |
| --- | --- | --- | --- |
| 1 | concise | The point and nothing else: one to three content slides. For a change, one slide with why, what and the main risk. | The claim in the title, at most 80 words, at most one excerpt of the ten or so lines that carry the idea. No edge cases and no alternatives. |
| 2 | focused | The key ideas only: about half of the standard slide budget. | About 150 words and one or two short excerpts. An edge case only when the goal depends on it. |
| 3 | standard | The normal slide budget: two to four slides for one concept or a small fix, six to eight for a subsystem or a typical feature. | One idea in about 250 words, the excerpts that teach it, a diagram where the idea has a shape, and a callout for each risk. |
| 4 | thorough | Also the secondary ideas and the edge cases that matter: about one and a half times the standard budget. Ideas that the standard deck puts in the appendix get slides. | The main edge cases and invariants, the alternatives the sources considered, and excerpts with their context lines; up to about 400 words. |
| 5 | complete | Everything the sources support: every idea a slide, and appendix groups explained, not only labeled. | All edge cases, invariants, failure modes and the reason for each decision, and the full function where it matters. A first paragraph that states the slide's point, then the detail. |

For a change (`walkthrough-pr`), no detail stop drops these, whatever its word count: the change's intent, every risk callout (one sentence each at stops 1 and 2), the excerpts of the hunks that the risks are about, coverage of every changed line (the appendix carries what the slides leave out), and the review findings behind Reveal.

## The default setting

- **Audience**: infer it, as step 1 of the skill infers the reader, and choose the stop whose "knows" column fits. A reviewer of a change in their own subsystem is usually at 4.
- **Detail**: 3. Stop 3 is the skill's normal budget, so a deck without arguments is the deck the skill wrote before the dials existed.

Tell the reader the setting with the URL, so that they can correct it.

## Arguments

The skill takes `--audience STOP` and `--detail STOP` anywhere in its arguments, where `STOP` is 1 to 5 or the stop's name: `--audience novice --detail 2`, or `--audience=4`. Read the topic, the pull request or the change from the rest. Explicit words about the level set the dials too: "for a newcomer" is audience 2, "explain it like I'm new to this" is audience 1, "briefly" is detail 2, "in full detail" is detail 5. Words about the scope, such as "walk me through everything in this PR", set no dial. Arguments win over words.

## Rewriting a slide

The page asks for a content slide at another setting, and `listen` hands you the request. `deck.py variant DECK SLIDE --audience A --detail D --brief` prints the whole brief for one rewrite: these rules, the stops' rows, the deck's outline, the slide as written and the checks that rely on it. Give it as is to a subagent, or follow it yourself. Rewrite the slide at the new setting:

- **Keep the slide's idea and id.** The title can change. Check slides and generated slides keep the deck's setting.
- **One slide, in place.** Do not add slides. At audience 1 or 2, put the background that a new slide would carry into the slide's first blocks, such as a callout with the definitions.
- **Change what the stops change.** The two settings must differ visibly: a novice slide defines terms that the specialist slide uses bare, and a concise slide is shorter and keeps the claim. When the sources do not support more detail, say on the slide what is not known rather than padding it.
- **Read the sources again when you need more.** Do not invent facts that the slide as written did not need.
- **Quote freely.** Any excerpt or diff range can be used; the deck as written keeps the coverage of a change.
- **Keep the prompts useful** at the new setting.
- **Keep the checks answerable.** Each check question whose `see` names the slide must still be settled by the rewrite, at every stop, concise included.

Write the slide object to a file and store it with `deck.py variant DECK SLIDE --audience A --detail D --file FILE`. It validates the slide like `check`, and an open page shows it at once. The variant stays valid until the slide as written, the lines it quotes, `reader` or `goal` change; `variant DECK` lists the stored variants, and `variant DECK --prune` deletes the stale ones.

### Rewriting a slide of a change

For `walkthrough-pr`, a rewrite follows the same rules as the slide as written:

- Keep each risk callout of the slide, in one sentence at detail 1 and 2, and the excerpt of the hunk it is about.
- Keep defects, recommendations and verdicts off the slide, at every stop, including detail 4 and 5 where the rewrite reads the sources again. A defect you notice becomes a review finding (`walkthrough_pr.py finding DECK add TEXT --review --severity PN --at PATH:LINE`), as in step 4 of walkthrough-pr.
- Label diagrams and examples of behavior as before or after the change, and say when an excerpt shows unchanged code for context.

### After storing

Do not cold-read a variant: the reader asked for it and reads it now, and the reader's questions do the cold reader's work.
