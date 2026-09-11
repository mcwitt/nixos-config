# shfmt parses syntax without executing it. Inspect literal command names;
# resolving variables, aliases, eval, or commands inside scripts is out of scope.
command=$(jq -r '.tool_input.command // ""')
if ! syntax=$(printf '%s\n' "$command" | shfmt -ln bash --to-json 2>/dev/null); then
  echo "agent-gh-guard: could not parse this shell command; use agent-gh for GitHub operations." >&2
  exit 0
fi

if jq -e '
  def literal:
    [.Parts[]? |
      if .Type == "Lit" or .Type == "SglQuoted" then .Value
      elif .Type == "DblQuoted" then literal
      else null end
    ] | if any(.[]; . == null) then null else join("") end;
  any(.. | objects | select(.Type? == "CallExpr");
    [.Args[]? | literal]
    | if .[0] == "command" then
        .[1:] | if .[0] == "-p" then .[1:] else . end
        | if .[0] == "--" then .[1:] else . end
      else . end
    | .[0] // "" | split("/") | last == "gh"
  )
' <<< "$syntax" >/dev/null; then
  echo "Use agent-gh for the agent identity. Use personal-gh only if the user explicitly authorized their personal identity for this action." >&2
  exit 2
fi
