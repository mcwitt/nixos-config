# Audience and detail

A deck is written at one setting of two dials, each with five stops. **Audience** sets how much background the deck assumes and how much it supplies. **Detail** sets how much of the material the deck covers and how deeply. The dials are independent: a novice can ask for a concise deck, which is short and still defines its terms, and an expert can ask for a complete one, which covers everything and defines nothing.

Record the setting in `deck.json` as `"audience": N` and `"detail": N` (1 to 5). The page then has a Level button in its top bar with both dials for the slide on screen, and the reader can ask for a slide, or all of them, at another setting. `deck.py` and `listen` quote the rows below; keep each row's text self-contained.

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

Word counts are prose and callouts on one content slide. Two things come on top of them, and win over the count when they conflict: definitions and background that the audience stop asks for (at audience 1 and detail 1, a slide still defines every term it uses), and, for a change, what the list below the table keeps. The final check of two to four questions stays at every stop (two at stops 1 and 2), even when the deck has three content slides or fewer and `check` does not require it.

| Stop | Name | The deck covers | One slide holds |
| --- | --- | --- | --- |
| 1 | concise | The point and nothing else: one to three content slides. For a change, one slide with why, what and the main risk. | The claim in the title, at most 80 words, at most one excerpt of the ten or so lines that carry the idea. No edge cases and no alternatives. |
| 2 | focused | The key ideas only: about half of the standard slide budget. | About 150 words and one or two short excerpts. An edge case only when the goal depends on it. |
| 3 | standard | The normal slide budget: two to four slides for one concept or a small fix, six to eight for a subsystem or a typical feature. | One idea in about 250 words, the excerpts that teach it, a diagram where the idea has a shape, and a callout for each risk. |
| 4 | thorough | Also the secondary ideas and the edge cases that matter: about one and a half times the standard budget. Ideas that the standard deck puts in the appendix get slides. | The main edge cases and invariants, the alternatives the sources considered, and excerpts with their context lines; up to about 400 words. |
| 5 | complete | Everything the sources support: every idea a slide, and appendix groups explained, not only labeled. | All edge cases, invariants, failure modes and the reason for each decision, and the full function where it matters. A first paragraph that states the slide's point, then the detail. |

For a change (`walkthrough-pr`), no detail stop drops these, whatever its word count: the change's intent, every risk callout (one sentence each at stops 1 and 2), the excerpts of the hunks that the risks are about, coverage of every changed line (the appendix carries what the slides leave out), and the review findings behind Reveal.

## The default setting

When the arguments and the request do not set a dial, infer it from the context: the conversation, what you remember of the reader (memory files, earlier decks and their settings) and what the reader asked for before. For audience, choose the stop whose "knows" column fits what you know of the reader; a reviewer of a change in their own subsystem is usually at 4. For detail, look for signs of how much the reader wants, such as a deadline, a quick look, or a request to understand something in full.

When the context does not settle a dial, use 3 for it: practitioner and standard. Stop 3 of detail is the skill's normal budget, so a deck with no arguments and no context is the deck the skill wrote before the dials existed.

Tell the reader the setting with the URL, so that they can correct it.

## Arguments

The skill takes `--audience STOP` and `--detail STOP` anywhere in its arguments, where `STOP` is 1 to 5 or the stop's name: `--audience novice --detail 2`, or `--audience=4`. Read the topic, the pull request or the change from the rest. Explicit words about the level set the dials too: "for a newcomer" is audience 2, "explain it like I'm new to this" is audience 1, "briefly" is detail 2, "in full detail" is detail 5. Words about the scope, such as "walk me through everything in this PR", set no dial. Arguments win over words.

## Rewriting a slide

The page asks for a slide at another setting, and `listen` hands you the request. A rewrite takes the path the deck took, write, cold-read, fix, and only then reaches the reader. Run the deck's script (`deck.py`, or `walkthrough_pr.py` for a change):

1. **Write.** `variant DECK SLIDE --audience A --detail D --brief` prints the whole brief for one rewrite: the rules below, the stops' rows, the deck's outline, the slide as written, and the slides or checks it relates to. Give it as is to a subagent, in parallel for several slides, or follow it yourself. The writer validates its draft with `--file FILE --check` and hands back the file; it does not store it.
2. **Cold-read.** `variant DECK --cold-read --candidate SLIDE A-D FILE …` prints the brief for a cold reader, who is the reader at the new setting: it reads the slides before the drafts as the reader saw them, then the drafts, and reports on the drafts only. Give several drafts of one request together. For a rewritten check, also run the blind reader with `--blind`. Run the readers as `cold-read.md` says, and fix the drafts as it says; one round, no second read.
3. **Store.** `variant DECK SLIDE --audience A --detail D --file FILE` validates the draft like `check` and stores it; an open page shows it at once.

The reader waits on the slide through all three steps, about a minute or two, so start the cold read as soon as the drafts pass `--check`, and answer questions that arrive meanwhile. A rewrite stays valid until the slide as written, the lines it quotes, the slides it covers (for a check), `reader` or `goal` change; `variant DECK` lists the stored rewrites.

### Rules for a rewrite

- **Keep the slide's idea and id.** The title can change. Generated slides (appendix groups, Findings, Proposed actions) keep the deck's setting.
- **One slide, in place.** Do not add slides. At audience 1 or 2, put the background that a new slide would carry into the slide's first blocks, such as a callout with the definitions.
- **Change what the stops change.** The two settings must differ visibly: a novice slide defines terms that the specialist slide uses bare, and a concise slide is shorter and keeps the claim. When the sources do not support more detail, say on the slide what is not known rather than padding it.
- **Read the sources again when you need more.** Do not invent facts that the slide as written did not need.
- **Quote freely.** Any excerpt or diff range can be used; the deck as written keeps the coverage of a change.
- **Keep the prompts useful** at the new setting.
- **Keep the checks answerable.** Each check question whose `see` names the slide must still be settled by the rewrite, at every stop, concise included.

### Rewriting a check

A check at another setting is a new set of questions for the reader who sees the deck at that setting. Its questions can change completely: their number, their kind, their wording and what they ask.

- **Keep what the check is for.** The final check spans the goal; a check after a run of slides covers those slides. Point each `see` at the content slide that settles the question; slide ids are the same at every setting.
- **Write from the slides as the reader sees them at this setting.** The brief gives them, rewritten where they have been. A novice check can rely on what the novice slides define; a concise check asks only what the concise slides say.
- **Audience sets what a question assumes.** At 1 and 2, define every term in the question itself and ask about the mechanism in plain words. At 4 and 5, use the subsystem's terms bare and ask for predictions on edge cases.
- **Detail sets the number and depth.** At 1, one or two questions on the main claim. At 3, as many as the check as written. At 5, up to six, with edge cases, and a written answer where choices would let the reader guess.
- **Follow Checks in `deck.md`.** Each question stands on its own; each wrong choice is a plausible misreading with a `why`; nothing gives the answer away. A written answer needs a rubric; a code question can use a test only from a test directory the deck already has.
- **Read each question once as a reader without the slides**, before the cold and blind readers see it. If the question or its choices give the answer away, rewrite it.

The page keeps the reader's answers per version of a check: answers to the check as written, or at another setting, stay with that version and come back when the reader switches back. Rewording a question starts a new version; changing only a choice's wording or a `why` does not. `listen` reports each check at the version the reader answered last.
