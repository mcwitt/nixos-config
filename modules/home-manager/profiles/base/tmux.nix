{
  config,
  lib,
  pkgs,
  ...
}:
let
  tmux = "${config.programs.tmux.package}/bin/tmux";

  # Agent state colors. Hex values from the base16 scheme do not depend on
  # the terminal's ANSI palette, which can differ from the scheme on hosts
  # whose terminal Stylix does not theme.
  stateColor =
    base: ansi: if config.stylix.enable then config.lib.stylix.colors.withHashtag.${base} else ansi;
  inputColor = stateColor "base08" "red";
  doneColor = stateColor "base0B" "green";
  backgroundColor = stateColor "base0D" "blue";

  # Agent harnesses set the pane option @agent_state; see
  # harnesses/tmux-state.nix. States, most urgent first.
  agentStates = [
    "input"
    "working"
    "background"
    "done"
  ];
  # The most urgent state in a space-separated list of states, such as one
  # rendered by a pane loop, or empty.
  mostUrgent =
    states: lib.foldr (state: rest: "#{?#{m:*${state}*,${states}},${state},${rest}}") "" agentStates;
  windowStates = "#{P:#{@agent_state} }";
  sessionStates = "#{W:${windowStates}}";
  # The glyph for a state, followed by a space: terminals may draw glyphs two
  # cells wide while tmux reserves one, which would hide the next character.
  glyph =
    state:
    "#{?#{==:${state},input},#[fg=${inputColor} bold]! #[default],#{?#{==:${state},done},#[fg=${doneColor}]✓ #[default],#{?#{==:${state},working},● ,#{?#{==:${state},background},#[fg=${backgroundColor}]◐ #[default],}}}}";
  # Count the agent panes in each state across all sessions.
  count = state: "#{n:#{S:#{W:#{P:#{?#{==:#{@agent_state},${state}},x,}}}}}";

  # Switch the client in $1 to the next pane, cycling in list order, whose
  # agent is blocked on input, else to the next one idle at its prompt.
  nextAgent = pkgs.writeShellScript "tmux-next-agent" ''
    client=$1
    current=$(${tmux} display -p -c "$client" '#{pane_id}')
    panes=$(${tmux} list-panes -a -F '#{pane_id} #{@agent_state}')
    for state in input done; do
      target=$(${pkgs.gawk}/bin/awk -v s="$state" -v c="$current" '
        $1 == c { past = 1; next }
        $2 == s { if (past) { print $1; exit } else if (!first) first = $1 }
        END { if (first) print first }
      ' <<<"$panes" | head -n1)
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

        # Agent state, set per pane by agent harness hooks as @agent_state:
        # "input" (blocked on a permission prompt or question), "done" (idle
        # at the prompt), "background" (idle, but waiting on background
        # subagents or shell commands) or "working". Windows and sessions
        # show the most urgent state among their panes.
        set -g @window_agent_state '${mostUrgent windowStates}'
        set -g @session_agent_state '${mostUrgent sessionStates}'
        set -g @window_agent_glyph '${glyph "#{E:@window_agent_state}"}'
        set -g @agents_working '${count "working"}'
        set -g @agents_background '${count "background"}'
        set -g @agents_done '${count "done"}'
        set -g @agents_input '${count "input"}'

        set -g status-left-length 30
        set -g status-right '#{?#{!=:#{E:@agents_working},0},  ● #{E:@agents_working},}#{?#{!=:#{E:@agents_background},0},  #[fg=${backgroundColor}]◐ #{E:@agents_background}#[default],}#{?#{!=:#{E:@agents_done},0},  #[fg=${doneColor}]✓ #{E:@agents_done}#[default],}#{?#{!=:#{E:@agents_input},0},  #[fg=${inputColor} bold]! #{E:@agents_input}#[default],} '
        set -g window-status-format '#I:#W#{?#{E:@window_agent_glyph}, #{E:@window_agent_glyph},}#{?window_flags,#{window_flags}, }'
        set -g window-status-current-format '#I:#W#{?#{E:@window_agent_glyph}, #{E:@window_agent_glyph},}#{?window_flags,#{window_flags}, }'

        set -g @tree_format '#{?pane_format,#{pane_current_command} ${glyph "#{@agent_state}"},#{?window_format,#{window_name}#{window_flags} #{E:@window_agent_glyph},#{session_windows}w#{?session_attached, (attached),} ${glyph "#{E:@session_agent_state}"}}}'
        bind s choose-tree -Zs -F '#{E:@tree_format}'
        bind w choose-tree -Zw -F '#{E:@tree_format}'
        # Jump to the next agent waiting for input, or list all waiting agents.
        bind a run-shell -b '${nextAgent} #{q:client_name}'
        bind A choose-tree -Zw -F '#{E:@tree_format}' -f '#{||:#{m:*input*,${windowStates}},#{m:*done*,${windowStates}}}'
      '';
    };
  };
}
