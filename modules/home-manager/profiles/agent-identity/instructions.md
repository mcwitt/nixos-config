## GitHub identity

Use `agent-gh` for GitHub operations. It uses the agent account. Plain `gh` and `personal-gh` use the human user's authentication.

Use `personal-gh` only when the user explicitly authorizes their personal identity for the relevant action. Existing authorization in the conversation is sufficient; do not ask again. Authorization for one action does not change the default identity for later actions. If personal identity is necessary and authorization is missing, ask before acting.

If `agent-gh` fails or lacks access, report the problem. Do not fall back to personal credentials, switch accounts, or bypass the identity hook. Use these commands instead of other GitHub integrations unless the integration's identity is known and appropriate for the action. Never print credentials.

These commands select GitHub CLI authentication only. They do not change Git commit authorship, signing, or push authentication.
