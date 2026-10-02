## GitHub identity

Use `agent-gh` for GitHub operations. It uses the agent account. Plain `gh` and `personal-gh` use the human user's authentication.

Use `personal-gh` only when the user explicitly authorizes their personal identity for the relevant action. Existing authorization in the conversation is sufficient; do not ask again. Authorization for one action does not change the default identity for later actions. If personal identity is necessary and authorization is missing, ask before acting.

If `agent-gh` fails or lacks access, report the problem. Do not fall back to personal credentials, switch accounts, or bypass the identity hook. Use these commands instead of other GitHub integrations unless the integration's identity is known and appropriate for the action. Never print credentials.

These commands select GitHub CLI authentication only. They do not change Git commit authorship, signing, or push authentication.

## Local commands in public artifacts

`agent-gh` and `personal-gh` are local wrappers around `gh`, not standard GitHub CLI commands. Treat other environment-specific aliases, shell functions, and helper commands the same way. When writing public repository content (including docs, skills, agent instructions, scripts, and examples) or public GitHub posts, use portable commands and describe any required authentication generically. For example, write `gh pr view`, even when you ran `agent-gh pr view` locally. Only name a local helper when the repository itself defines and documents it or the user explicitly asks to discuss it.

This is a portability and relevance rule, not a security or secrecy restriction. Continue using the required identity wrappers when executing commands locally; public examples using `gh` do not authorize bypassing the identity rules above. Before committing or publishing, check the content you changed for local command names and replace incidental references with their portable equivalents.
