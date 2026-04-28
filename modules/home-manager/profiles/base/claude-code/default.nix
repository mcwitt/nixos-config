{
  config,
  inputs,
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
      local pct="$1" label="$2" reset_at="$3"
      local pct_int=''${pct%.*}
      local color=""
      if (( pct_int >= 85 )); then
        color="$red"
      elif (( pct_int >= 60 )); then
        color="$yellow"
      fi
      local label_color="''${color:-$chrome}"
      printf '%s%s%s%s %s%.0f%%%s' "$sep" "$label_color" "$label" "$reset" "$color" "$pct" "$reset"
      if [[ -n "$reset_at" ]]; then
        local remaining=$(( ''${reset_at%.*} - now ))
        printf ' %s%s%s' "$chrome" "$(fmt_duration "$remaining")" "$reset"
      fi
    }

    printf '%s%s%s' "$abbreviated" "$sep" "$model"
    fmt_pct "$ctx" "ctx"
    fmt_pct "$five_h" "5h" "$five_h_reset"
    fmt_pct "$seven_d" "7d" "$seven_d_reset"
  '';
in
{
  config = lib.mkIf cfg.enable {

    home.packages = [
      pkgs.sox # for voice mode
    ];

    programs.claude-code = {
      enable = true;

      settings = {
        model = "claude-opus-4-7";
        effortLevel = "xhigh";
        permissions.defaultMode = "auto";
        skipAutoPermissionPrompt = true;
        editorMode = "vim";
        voiceEnabled = true;
        theme = if config.stylix.polarity == "dark" then "dark" else "light";
        statusLine = {
          type = "command";
          command = "${statuslineScript}";
        };
        hooks = lib.mkIf config.programs.peon-ping.enable {
          Notification = [
            {
              hooks = [
                {
                  type = "command";
                  command = "${config.programs.peon-ping.package}/bin/peon";
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

      skills = {
        nixify = ./skills/nixify;
      };
    };
  };
}
