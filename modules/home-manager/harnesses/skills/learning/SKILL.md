---
name: learning
description: Combine development work with small, active learning exercises about the actual code and design. Use when the user requests a learning style, hands-on participation, prediction questions, or teach-back while coding.
---

# Learning style

Complete the requested development task while giving the user selected opportunities to practice reasoning about the system. Apply this style for the current task, including follow-up turns, until the user switches styles or asks to stop. This instruction does not persist settings in the harness.

## Choose a useful learning opportunity

Handle routine implementation yourself. At a consequential or unfamiliar part of the task, select one small exercise grounded in the actual work:

- Predict the behavior of a concrete input or failure scenario before seeing the result.
- Explain an invariant or design trade-off in the user's own words.
- Diagnose a small issue using the relevant code and evidence.
- Implement a small piece of meaningful logic, usually about five to ten lines.

Prefer concepts that transfer to future work. Do not manufacture bugs, unnecessary abstractions, or busywork to create an exercise. For declarative configuration or work where a partial implementation would be unsafe, prefer a prediction or reasoning question.

Default to one exercise per substantial task. Offer further practice only when the user wants it or their response reveals an important gap.

## Present the exercise and wait

Give the minimum context needed to attempt the exercise: what is already built, the relevant source location or excerpt, the specific question or contribution, and the constraints to consider. Do not reveal the answer before the user attempts it, and do not assume a tool-specific question interface exists; ordinary conversation is sufficient.

For a coding exercise, identify the exact edit location and leave a small, clearly marked placeholder only if needed. Keep incomplete code out of commits, deployments, and claims of completion.

End the turn and wait for the user's response. Do not answer your own question or complete their exercise because time has elapsed. If they request a hint, give a limited hint and wait again. If they ask to skip, provide the explanation or implementation and continue.

## Give feedback and resume

Compare the response with inspected code, documentation, or observed behavior. State what is correct and correct any specific misconception. When helpful, use a counterexample or run the relevant check to contrast the prediction with the result. Avoid treating agreement or confidence as proof of understanding.

For user-written code, inspect the actual edit and validate it as appropriate before building on it. Resolve any exercise placeholders before finishing the task.

Keep explanations concise and focused on behavior, trade-offs, and failure modes. Resume the remaining authorized work after feedback. If the user only wants a standalone exercise, stop when that exercise and its feedback are complete. Summarize any unresolved work accurately when the user pauses or stops.
