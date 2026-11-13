# The cold read

By the time you write the deck you know the material, so you cannot see what the deck leaves unexplained: a term the sources introduce, a name from the work behind them, an abbreviation, a fact you learned while reading and then used. A reader who starts with none of that can see it. The cold read gives the finished draft to such a reader, and you fix the deck where they stopped understanding.

## 1. State the reader

Set `reader` in `deck.json` from the starting point you settled in step 1, as two sentences: what the reader knows, then what they are new to.

```json
"reader": "Knows: Python, Docker, and how the project runs training jobs. New to: Lean and theorem proving, the task-curation pipeline, and this change."
```

- **Knows** stops the cold reader from asking about the reader's own field. Name the project's parts the reader works with, with a few words on each, because the cold reader does not know the project either. Add the people and teams the reader works with when the deck names them.
- **New to** matters as much. The cold reader is a model, and it possibly knows a subject that the reader does not. It reports an unexplained term from a subject only when the statement says that the reader is new to that subject.
- Write only what the reader knew before this deck's subject existed. A term you learned from the sources does not belong in either sentence.
- Match the audience stop (see `dials.md`): "Knows" covers the layers the stop names. `text --brief` gives the cold reader the deck's audience and detail stops, so that it does not report what the detail stop leaves out by design.

## 2. Run the two readers

```sh
python3 <skill-dir>/scripts/deck.py text DECK --brief > DECK/cold-read.md
python3 <skill-dir>/scripts/deck.py text DECK --brief --checks-only > DECK/blind-checks.md
```

`text` prints the deck as the page shows it, in slide order: prose, callouts, excerpts with their lines, diagram labels, and the check questions without their answers. `--brief` puts the reader's instructions and your `reader` statement before it. Do not add to the brief or summarize the deck for the reader: each sentence you add carries what you know.

Start both readers at the same time and wait for both:

- **The cold reader** gets `cold-read.md`. It reads the slides in order and returns one report: a one-sentence summary of each slide, at most 12 gaps, its answers to the checks, and whether it reached the goal.
- **The blind reader** gets `blind-checks.md`: the check questions with no deck. It shows which questions a reader can answer without the slides. Skip it for a deck with no check.

Each reader must start with none of your context. Use the first of these that your harness has:

1. A subagent that starts fresh. Tell it to read that one file, open nothing else, and return the report. Do not use a fork or a subagent that inherits this conversation.
2. A new non-interactive session of your harness's command-line program, started in an empty directory, with the file as its prompt.
3. Neither: do the cold read yourself, and tell the user that it is weaker. Run `text DECK`, and for each name, abbreviation, identifier and number in the prose, find its first use and check that the same slide or an earlier one says what it is. Then answer each check from the slides alone. You cannot do the blind read.

The readers see only the deck. They do not get the sources, the diff, the conversation or your notes. That keeps them at the reader's level, and it leaves them nothing to review but the explanation.

## 3. Fix the deck

Every item in the report is about the deck. Do not answer a question in your head and move on: a question the cold reader asks is one the reader would also ask, and the answer belongs on a slide. Go through the report in this order.

**Summaries.** Compare each with the idea you wrote the slide to teach. A summary that is wrong, that is "unclear", or that names a minor point means that the slide does not teach its idea, even when the reader reported no gap there.

**Gaps.** Decide for each one:

| The gap | What to do |
|---|---|
| The reader needs it for the goal, and the deck does not say it | Fix the deck: see below |
| The reader's guess (`slows`) is wrong | The same. A wrong guess is worse than a question, because the reader does not know to ask |
| The stated reader already knows it | Change nothing in the slides. If you are not sure that the real reader knows it, ask the user. If several gaps are of this kind, `reader` was incomplete: correct it |
| The goal does not need it | Change nothing, or remove the detail that raised the question |
| It is about the subject, not the explanation (a possible defect, a design opinion) | Do not use it in the slides |

Fix a real gap with the smallest of these that works:

1. **Use plain words** in place of the term, when the term is only a label from the work and the reader does not need it later.
2. **Define the term where it first appears**, in one clause or sentence. That is often an earlier slide than the one where the reader stopped. Check with `text DECK` that no slide before the definition uses the term.
3. **Move the slide** that explains it before the slide that needs it (kind `order`).
4. **Add a background slide** when several gaps come from one missing idea. Keep the slide budget: move or cut a slide the goal needs less.
5. **Cut** the passage, when the goal does not need what it says.

**Checks.** The cold reader's answers measure the checks more than the deck: a reader can report a gap that blocks a slide and still answer every question. So use the gaps to fix the slides, and use this part to fix the checks. Grade the cold reader's answers against `text DECK --checks-only --key`, and written answers against their rubric. Then read the two readers together:

| Cold reader | Blind reader | It means | What to do |
|---|---|---|---|
| Right, from the slide in `see` | Wrong or unsure | The check works | Nothing |
| Right | Right, with high confidence | The question gives its answer away, or needs no deck | Make each wrong choice a plausible misreading of the same length and detail as the right one, or ask for a prediction on a concrete input |
| Right, basis `wording` | either | It tests the slide's phrases | Ask what the material implies, in other words than the slide's |
| Wrong, or basis `guess` | either | The deck does not give what the question needs, or the question has two readings | Read the reader's basis. Fix the slide in `see`, or reword the question. Do not move the answer into the question |
| Wrong, and the slides do settle it | either | A hard question, possibly a good one | Keep it if a careful reader gets it from the slides. Otherwise show the missing step |
| Right, and the basis of every question is one slide | either | The final check does not span the deck | Replace one question with one that needs two slides together |

A blind reader that is right with low confidence shows little: with three choices, a guess is right one time in three, and a capable reader can reason from a question that has to describe its situation. Revise the questions it answers with high confidence, one time, and do not run the blind reader again to confirm.

Run `check` again after the edits.

## 4. When to stop

One round is the default. Run a second cold read, with a new reader, only when you added, removed or reordered slides, or rewrote most of one. The first reader is no longer cold, so do not send it the new deck. Do not run a third.

Skip the cold read when the deck has three content slides or fewer, when the user asks for the deck now, or when you revise a deck during the walkthrough: there, the reader's own questions do this work.

When you give the reader the URL, say in one sentence what the cold read changed.

## What to watch for

- **A long list of small gaps.** The report holds at most 12, the most serious first. If the first ones are minor, the deck is in good shape. Do not make changes only to use the list.
- **Questions about the reader's own field.** `reader` did not cover it. Correct the statement instead of adding definitions the reader will skim.
- **Opinions about the subject.** The brief forbids them, and the readers have no sources to judge. Drop any that appear.
- **A report with terms only.** Terms are the easiest gaps to find. If no gap has the kind `why` or `question`, read the summaries again for a slide that says what was done and not the reason.
- **A deck that grows.** Each fix adds words. After the fixes, check the slide budget and that each slide still has one idea.
