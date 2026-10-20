# The coordinator role: the shared `coordinator` skill, its per-harness
# scoping, and `agent-coordinator`, which starts or reattaches the
# coordinator's own tmux session.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.harnesses;
  skill = ./skills/coordinator;
  home = config.home.homeDirectory;

  # Where each harness installs its skills (see the harness modules).
  skillDirs = {
    claude = "${config.programs.claude-code.configDir}/skills/coordinator";
    codex = "${home}/.codex/skills/coordinator";
    pi = "${home}/.pi/agent/skills/coordinator";
    opencode = "${config.xdg.configHome}/opencode/skills/coordinator";
    antigravity = "${home}/.gemini/config/skills/coordinator";
  };
  # Appended to the system prompt where the harness allows it, so the role
  # survives compaction and resumes. No quotes or backslashes: it goes into a
  # Codex TOML string as is.
  note =
    dir:
    "This session is the coordinator: it triages the user's requests, handles small ones itself and launches a dedicated tmux session for each substantial one, following the coordinator skill. Its helper is ${dir}/scripts/coordinator.py. If the skill's instructions are no longer in your context, for example after compaction or on resume, read ${dir}/SKILL.md before acting on the next request. These instructions apply to this session only; do not record them as memory or in any shared instructions file.";

  launcher = pkgs.writeShellScriptBin "agent-coordinator" ''
    set -eu
    PATH=${lib.makeBinPath [ config.programs.tmux.package ]}:$PATH
    helper() { ${lib.getExe pkgs.python3} ${skill}/scripts/coordinator.py "$@"; }
    usage() {
      echo "usage: agent-coordinator [claude|codex|pi|opencode|antigravity] [--resume]" >&2
      exit 2
    }
    harness=claude resume=
    for arg; do
      case $arg in
        --resume) resume=1 ;;
        -*) usage ;;
        *) harness=$arg ;;
      esac
    done
    attach() {
      if [ -n "''${TMUX:-}" ]; then exec tmux switch-client -t =coordinator; fi
      exec tmux attach-session -t =coordinator
    }
    tmux has-session -t =coordinator 2>/dev/null && attach
    id=
    if [ -n "$resume" ]; then id=$(helper show coordinator --field session_id 2>/dev/null || true); fi
    launch() { helper launch coordinator --dir "$PWD" "$@"; }

    # Start, or with --resume continue, the coordinator in each harness. The
    # first prompt invokes the skill by name.
    case $harness in
      claude)
        n=${lib.escapeShellArg (note skillDirs.claude)}
        if [ -n "$id" ]; then
          launch --attach --session-id "$id" -- claude --resume '{session_id}' --append-system-prompt "$n"
        elif [ -n "$resume" ]; then
          launch --attach -- claude --continue --append-system-prompt "$n"
        else
          launch --attach --prompt /coordinator -- \
            claude --name coordinator --session-id '{session_id}' --append-system-prompt "$n" -- '{prompt}'
        fi
        ;;
      codex)
        c=${lib.escapeShellArg ''developer_instructions="${note skillDirs.codex}"''}
        if [ -n "$resume" ]; then
          launch --attach -- codex resume --no-daemon --last -c "$c"
        else
          launch --attach --prompt '$coordinator' -- codex --no-daemon -c "$c" -- '{prompt}'
        fi
        ;;
      pi)
        n=${lib.escapeShellArg (note skillDirs.pi)}
        if [ -n "$id" ]; then
          launch --attach --session-id "$id" -- pi --session '{session_id}' --append-system-prompt "$n"
        elif [ -n "$resume" ]; then
          launch --attach -- pi --continue --append-system-prompt "$n"
        else
          launch --attach --prompt /skill:coordinator -- \
            pi --name coordinator --session-id '{session_id}' --append-system-prompt "$n" -- '{prompt}'
        fi
        ;;
      opencode)
        # A leading / in --prompt is not run as a command, so type it in.
        if [ -n "$resume" ]; then
          launch --attach -- opencode . --continue
        else
          launch --wait 8 -- opencode .
          helper send coordinator /coordinator
          attach
        fi
        ;;
      antigravity | agy)
        if [ -n "$resume" ]; then
          launch --attach -- agy --continue
        else
          launch --attach --prompt /coordinator -- agy '--prompt-interactive={prompt}'
        fi
        ;;
      *) usage ;;
    esac
  '';
in
{
  config = lib.mkIf cfg.enable {
    harnesses.skills.coordinator = skill;
    home.packages = [ launcher ];

    # Keep the skill out of ordinary sessions. Claude Code and pi honor
    # disable-model-invocation in SKILL.md and Codex reads agents/openai.yaml.
    # OpenCode ignores both; denying the skill tool hides it from the model,
    # while typing /coordinator still runs it.
    programs.opencode.settings.permission.skill.coordinator = "deny";
  };
}
