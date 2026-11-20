# Wraps `agy` to publish each session's state as the tmux pane option
# @agent_state; see ../tmux-state.nix for the states and the script.
#
# Antigravity's hooks cover the start and end of a turn but not a permission
# prompt, a question or the folder-trust dialog. Its terminal status reports
# (OSC 7501) cover all of them, so the wrapper turns them on and has tmux pipe
# the pane's output to ./tmux-state.py, which publishes each report and clears
# the state when `agy` exits. tmux itself discards the reports.
{
  config,
  pkgs,
  package,
}:
let
  tmux = "${config.programs.tmux.package}/bin/tmux";
  shared = import ../tmux-state.nix { inherit config pkgs; };
  watcher = pkgs.writeScript "agy-tmux-state" ''
    #!${pkgs.python3}/bin/python3
    ${builtins.readFile ./tmux-state.py}
  '';

  # Only for a session that owns the pane: `agy` run by another agent in its
  # pane inherits $TMUX_PANE, but not the pane's terminal. A pane has one
  # pipe, so leave one that is already in use alone.
  wrapper = pkgs.writeShellScript "agy" ''
    if [[ -n "''${TMUX:-}" && -n "''${TMUX_PANE:-}" && -t 0 && -t 1 && "''${AGY_CLI_OSC7501:-}" != off ]]; then
      pane=$(${tmux} display -p -t "$TMUX_PANE" '#{pane_tty} #{pane_pipe}' 2>/dev/null)
      if [[ "$pane" == "$(${pkgs.coreutils}/bin/tty) 0" ]]; then
        # tmux does not answer the CLI's check for support.
        export AGY_CLI_OSC7501=on
        ${tmux} pipe-pane -t "$TMUX_PANE" \
          "exec ${watcher} ${shared} ${tmux} '#{socket_path}' '#{pane_id}' $$"
      fi
    fi
    exec ${package}/bin/agy "$@"
  '';
in
pkgs.symlinkJoin {
  inherit (package) pname version meta;
  paths = [ package ];
  postBuild = ''
    rm $out/bin/agy
    ln -s ${wrapper} $out/bin/agy
  '';
}
