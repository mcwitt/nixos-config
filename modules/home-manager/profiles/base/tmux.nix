{ config, lib, ... }:
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

        # Ghostty supports truecolor and OSC 52, including over SSH.
        set -as terminal-features ',xterm-ghostty:RGB:clipboard'
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
      '';
    };
  };
}
