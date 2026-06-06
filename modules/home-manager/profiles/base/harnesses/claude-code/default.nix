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
    sep=" $chrome·$reset "

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

    # --- worktree git segment (local-only; instant, no network) ---
    # Rendered in the existing theme ($sep / $chrome / $yellow / $red / $reset).
    if [[ "$(${pkgs.git}/bin/git -C "$cwd" rev-parse --is-inside-work-tree 2>/dev/null)" == "true" ]]; then
      branch=$(${pkgs.git}/bin/git -C "$cwd" branch --show-current 2>/dev/null)
      [[ -z "$branch" ]] && branch="detached"

      # dirty: any staged/unstaged/untracked change
      dirty=""
      [[ -n "$(${pkgs.git}/bin/git -C "$cwd" status --porcelain 2>/dev/null)" ]] && dirty="!"

      # base branch for "ahead": prefer origin/HEAD, else local main/master
      base=$(${pkgs.git}/bin/git -C "$cwd" symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null)
      if [[ -z "$base" ]]; then
        for b in main master; do
          ${pkgs.git}/bin/git -C "$cwd" rev-parse --verify -q "$b" >/dev/null 2>&1 && { base="$b"; break; }
        done
      fi
      ahead=""
      [[ -n "$base" ]] && ahead=$(${pkgs.git}/bin/git -C "$cwd" rev-list --count "$base"..HEAD 2>/dev/null)

      # unpushed vs upstream
      unpushed=$(${pkgs.git}/bin/git -C "$cwd" rev-list --count '@{upstream}..HEAD' 2>/dev/null)

      # working diff vs HEAD (insertions/deletions) — off-the-rails gauge.
      # NOTE: untracked files aren't counted by `diff`; they still trip `!` above.
      shortstat=$(${pkgs.git}/bin/git -C "$cwd" diff HEAD --shortstat 2>/dev/null)
      ins=$(grep -oE '[0-9]+ insertion' <<<"$shortstat" | grep -oE '[0-9]+'); ins=''${ins:-0}
      del=$(grep -oE '[0-9]+ deletion' <<<"$shortstat" | grep -oE '[0-9]+'); del=''${del:-0}

      # worktrunk activity marker (git config JSON: {"marker":"💬",...})
      marker=""
      raw=$(${pkgs.git}/bin/git -C "$cwd" config --get "worktrunk.state.$branch.marker" 2>/dev/null)
      [[ -n "$raw" ]] && marker=$(${pkgs.jq}/bin/jq -r '.marker // empty' <<<"$raw" 2>/dev/null)

      # --- render ---
      printf '%s%s%s' "$sep" "$chrome" "$branch"
      [[ -n "$marker" ]] && printf ' %s' "$marker"
      printf '%s' "$reset"
      [[ -n "$dirty" ]] && printf ' %s%s%s' "$yellow" "$dirty" "$reset"
      [[ -n "$ahead" && "$ahead" -gt 0 ]] && printf ' %s↑%s%s' "$chrome" "$ahead" "$reset"
      [[ -n "$unpushed" && "$unpushed" -gt 0 ]] && printf ' %s⇡%s%s' "$chrome" "$unpushed" "$reset"
      if (( ins > 0 || del > 0 )); then
        dcolor="$chrome"
        (( ins + del >= 500 )) && dcolor="$yellow"
        (( ins + del >= 2000 )) && dcolor="$red"
        printf ' %s+%s -%s%s' "$dcolor" "$ins" "$del" "$reset"
      fi
    fi
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
      };

      plugins = with inputs; [
        "${autoresearch}/claude-plugin"
        superpowers
        # v0.50.0 plugin root is the repo root: .claude-plugin/plugin.json there
        # references ./skills/{worktrunk,wt-switch-create} and ./.claude-plugin/hooks.
        "${pkgs.worktrunk.src}"
      ];

      skills = localSkills // gwsSkills;
    };
  };
}
