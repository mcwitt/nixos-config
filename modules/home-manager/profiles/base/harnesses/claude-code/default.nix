{
  config,
  gwsSkills,
  inputs,
  localSkills,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.profiles.base;
  rgbTriple =
    name:
    let
      colors = config.lib.stylix.colors;
    in
    "${colors."${name}-rgb-r"};${colors."${name}-rgb-g"};${colors."${name}-rgb-b"}";
  chromeRgb = rgbTriple "base04";
  yellowRgb = rgbTriple "base0A";
  redRgb = rgbTriple "base08";
  agentNotifyApp = pkgs.writeShellApplication {
    name = "claude-agent-notify";
    # Only jq/libnotify/coreutils are pinned; `claude` (for `claude agents
    # --json`) is resolved from the session PATH at hook runtime (the hook always
    # runs inside a live claude process), which avoids a closure dep / eval cycle.
    runtimeInputs = [
      pkgs.jq
      pkgs.libnotify
      pkgs.coreutils
    ];
    text = builtins.readFile ./claude-agent-notify.sh;
  };
  # Audio cue for claude-agent-notify (empty => no sound), passed via env so the
  # script stays pure/testable. peon-ping is retained purely as this sound source.
  agentNotifySound = lib.optionalString config.programs.peon-ping.enable "${config.programs.peon-ping.package}/bin/peon";
  agentNotify =
    mode:
    "CLAUDE_AGENT_NOTIFY_SOUND=${agentNotifySound} ${agentNotifyApp}/bin/claude-agent-notify ${mode}";
  statuslineScript = pkgs.writeShellScript "claude-statusline" ''
    mapfile -t f < <(
      ${pkgs.jq}/bin/jq -r '
        .workspace.current_dir,
        .model.display_name,
        (.context_window.used_percentage // ""),
        (.rate_limits.five_hour.used_percentage // ""),
        (.rate_limits.five_hour.resets_at // ""),
        (.rate_limits.seven_day.used_percentage // ""),
        (.rate_limits.seven_day.resets_at // "")
      '
    )
    cwd=''${f[0]} model=''${f[1]} ctx=''${f[2]}
    five_h=''${f[3]} five_h_reset=''${f[4]}
    seven_d=''${f[5]} seven_d_reset=''${f[6]}

    if [[ "$cwd" == "$HOME" ]]; then
      display_path="~"
    elif [[ "$cwd" == "$HOME/"* ]]; then
      display_path="~/''${cwd#"$HOME/"}"
    else
      display_path="$cwd"
    fi

    IFS='/' read -ra parts <<< "$display_path"
    abbreviated=""
    total=''${#parts[@]}
    for ((i = 0; i < total - 1; i++)); do
      part="''${parts[$i]}"
      if [[ -z "$part" ]]; then
        abbreviated+="/"
      elif [[ "$part" == "~" ]]; then
        abbreviated+="~/"
      else
        abbreviated+="''${part:0:1}/"
      fi
    done
    abbreviated+="''${parts[$((total - 1))]}"

    chrome=$'\033[38;2;${chromeRgb}m'
    yellow=$'\033[38;2;${yellowRgb}m'
    red=$'\033[38;2;${redRgb}m'
    reset=$'\033[0m'
    sep=" $chrome|$reset "

    fmt_duration() {
      local secs="$1"
      if (( secs <= 0 )); then
        printf "now"
        return
      fi
      local d=$((secs / 86400))
      local h=$(( (secs % 86400) / 3600 ))
      local m=$(( (secs % 3600) / 60 ))
      if (( d > 0 )); then
        printf "%dd%02dh" "$d" "$h"
      elif (( h > 0 )); then
        printf "%dh%02dm" "$h" "$m"
      else
        printf "%dm" "$m"
      fi
    }

    printf -v now '%(%s)T' -1

    fmt_pct() {
      [[ -z "$1" ]] && return 0
      local pct="$1" label="$2" yellow_at="$3" red_at="$4" reset_at="$5"
      local pct_int=''${pct%.*}
      local color=""
      if (( pct_int >= red_at )); then
        color="$red"
      elif (( pct_int >= yellow_at )); then
        color="$yellow"
      fi
      local label_color="''${color:-$chrome}"
      printf '%s%s%s%s %s%.0f%%%s' "$sep" "$label_color" "$label" "$reset" "$color" "$pct" "$reset"
      if [[ -n "$reset_at" ]]; then
        local remaining=$(( ''${reset_at%.*} - now ))
        printf ' %s%s%s' "''${color:-$chrome}" "$(fmt_duration "$remaining")" "$reset"
      fi
    }

    printf '%s%s%s' "$abbreviated" "$sep" "$model"
    fmt_pct "$ctx" "ctx" 40 60
    fmt_pct "$five_h" "5h" 60 85 "$five_h_reset"
    fmt_pct "$seven_d" "7d" 60 85 "$seven_d_reset"
  '';
in
{
  config = lib.mkIf cfg.enable {

    home.packages = [
      pkgs.sox # for voice mode
    ];

    programs.claude-code = {
      enable = true;
      enableMcpIntegration = true;

      settings = {
        model = "claude-opus-4-8";
        # `effortLevel` alone is silently ignored on Opus 4.x: a per-model
        # "launch effort pin" (unpinOpus4{7,8}LaunchEffort, stored in the
        # mutable ~/.claude.json) forces the model's default effort (high on
        # 4.8) until you run `/effort` once. But `/effort` writes the new level
        # into ~/.claude/settings.json *before* clearing the pin, and that path
        # is a read-only Nix store symlink, so it dies with EROFS and never
        # unpins. The CLAUDE_CODE_EFFORT_LEVEL env var sidesteps the pin
        # entirely (top precedence in the resolver) and is model-agnostic, so
        # it survives future model bumps. See anthropics/claude-code#52534.
        # `effortLevel` is kept for documentation / as the unpinned fallback.
        effortLevel = "xhigh";
        env.CLAUDE_CODE_EFFORT_LEVEL = "xhigh";
        permissions.defaultMode = "auto";
        skipAutoPermissionPrompt = true;
        editorMode = "vim";
        voiceEnabled = true;
        theme = if config.stylix.polarity == "dark" then "dark" else "light";
        statusLine = {
          type = "command";
          command = "${statuslineScript}";
        };
        hooks = lib.optionalAttrs config.profiles.desktop.enable {
          # Fires when the agent is blocked waiting on the user. No matcher =>
          # fires on every Notification event (audio + dunst, both via
          # claude-agent-notify); a matcher can be added later to drop e.g. auth
          # pings.
          Notification = [
            {
              hooks = [
                {
                  type = "command";
                  command = agentNotify "needs-input";
                }
              ];
            }
          ];
          # Fires on turn-end; the script no-ops unless CLAUDE_JOB_DIR is set
          # (i.e. only background agent-view sessions, not foreground ones).
          Stop = [
            {
              hooks = [
                {
                  type = "command";
                  command = agentNotify "done";
                }
              ];
            }
          ];
        };
      };

      plugins = with inputs; [
        "${autoresearch}/claude-plugin"
        superpowers
      ];

      skills = localSkills // gwsSkills;
    };
  };
}
