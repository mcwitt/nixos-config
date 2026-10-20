{
  config,
  lib,
  pkgs,
  ...
}:
let
  tmux = "${config.programs.tmux.package}/bin/tmux";

  # Switch the client in $1 to the next window, cycling in list order, whose
  # agent is blocked on input, else to the next one idle at its prompt.
  # Claude Code hooks set @agent_state; see harnesses/claude-code/tmux-state.nix.
  nextAgent = pkgs.writeShellScript "tmux-next-agent" ''
    client=$1
    current=$(${tmux} display -p -c "$client" '#{window_id}')
    windows=$(${tmux} list-windows -a -F '#{window_id} #{@agent_state}')
    for state in input done; do
      target=$(${pkgs.gawk}/bin/awk -v s="$state" -v c="$current" '
        $1 == c { past = 1; next }
        $2 == s { if (past) { print $1; exit } else if (!first) first = $1 }
        END { if (first) print first }
      ' <<<"$windows" | head -n1)
      if [[ -n "$target" ]]; then
        exec ${tmux} switch-client -c "$client" -t "$target"
      fi
    done
    ${tmux} display-message -c "$client" "No agent is waiting"
  '';
in
{
  config = lib.mkIf config.profiles.base.enable {
    programs.tmux = {
      enable = true;
      # Match the interactive shell on Linux and macOS.
      shell = lib.getExe config.programs.fish.package;
      terminal = "tmux-256color";
      # Keep C-b so C-a remains available for Emacs beginning-of-line.
      shortcut = "b";
      keyMode = "vi";
      customPaneNavigationAndResize = true;
      baseIndex = 1;
      mouse = true;
      focusEvents = true;
      escapeTime = 10;
      historyLimit = 50000;
      clock24 = true;

      # Stylix supplies the status-line colors when enabled.
      extraConfig = ''
        set -g renumber-windows on

        set -as terminal-features ',xterm-ghostty:RGB:clipboard:extkeys'
        set -g extended-keys on
        set -g extended-keys-format csi-u
        # Preserve clipboard writes from terminal Emacs and nested tmux.
        # Applications inside tmux can therefore also write the clipboard.
        set -s set-clipboard on

        # Keep new panes/windows in the active pane's project or worktree.
        bind c new-window -c '#{pane_current_path}'
        bind % split-window -h -c '#{pane_current_path}'
        bind '"' split-window -v -c '#{pane_current_path}'
        bind | split-window -h -c '#{pane_current_path}'
        bind - split-window -v -c '#{pane_current_path}'

        bind -T copy-mode-vi v send-keys -X begin-selection
        bind -T copy-mode-vi y send-keys -X copy-selection-and-cancel

        # Agent state, set per window by Claude Code hooks as @agent_state:
        # "input" (blocked on a permission prompt or question), "done" (idle
        # at the prompt), "background" (idle, but waiting on background
        # subagents or shell commands) or "working".
        # Glyphs are followed by a space: terminals may draw them two cells
        # wide while tmux reserves one, which would hide the next character.
        set -g @agent_glyph '#{?#{==:#{@agent_state},input},#[fg=red bold]! #[default],#{?#{==:#{@agent_state},done},#[fg=yellow]✓ #[default],#{?#{==:#{@agent_state},working},● ,#{?#{==:#{@agent_state},background},#[fg=cyan]◐ #[default],}}}}'
        # Count agents in each state; prefix A lists the waiting ones.
        set -g @agents_working '#{n:#{S:#{W:#{?#{==:#{@agent_state},working},x,}}}}'
        set -g @agents_background '#{n:#{S:#{W:#{?#{==:#{@agent_state},background},x,}}}}'
        set -g @agents_done '#{n:#{S:#{W:#{?#{==:#{@agent_state},done},x,}}}}'
        set -g @agents_input '#{n:#{S:#{W:#{?#{==:#{@agent_state},input},x,}}}}'

        set -g status-left-length 30
        set -g status-right '#{?#{!=:#{E:@agents_working},0},  ● #{E:@agents_working},}#{?#{!=:#{E:@agents_background},0},  #[fg=cyan]◐ #{E:@agents_background}#[default],}#{?#{!=:#{E:@agents_done},0},  #[fg=yellow]✓ #{E:@agents_done}#[default],}#{?#{!=:#{E:@agents_input},0},  #[fg=red bold]! #{E:@agents_input}#[default],} '
        set -g window-status-format '#I:#W#{?#{E:@agent_glyph}, #{E:@agent_glyph},}#{?window_flags,#{window_flags}, }'
        set -g window-status-current-format '#I:#W#{?#{E:@agent_glyph}, #{E:@agent_glyph},}#{?window_flags,#{window_flags}, }'

        set -g @tree_format '#{?pane_format,#{pane_current_command},#{?window_format,#{window_name}#{window_flags},#{session_windows}w#{?session_attached, (attached),}}}#{?pane_format,, #{E:@agent_glyph}}'
        bind s choose-tree -Zs -F '#{E:@tree_format}'
        bind w choose-tree -Zw -F '#{E:@tree_format}'
        # Jump to the next agent waiting for input, or list all waiting agents.
        bind a run-shell -b '${nextAgent} #{q:client_name}'
        bind A choose-tree -Zw -F '#{E:@tree_format}' -f '#{||:#{==:#{@agent_state},input},#{==:#{@agent_state},done}}'
      '';
    };
  };
}
