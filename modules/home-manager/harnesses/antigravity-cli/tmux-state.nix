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
  # pipe, so leave one that is in use alone. One whose process has gone, or
  # is the watcher of an `agy` that has exited, is not in use: tmux keeps the
  # pipe of a dead pane, and a watcher takes a moment to clean up.
  wrapper = pkgs.writeShellScript "agy" ''
    free() {
      [[ "$piped" == 0 ]] && return
      local args
      args=$(${pkgs.procps}/bin/ps -o args= -p "$pipe_pid" 2>/dev/null) || return 0
      [[ "$args" == *"${watcher} "* ]] && ! kill -0 "''${args##* }" 2>/dev/null
    }
    if [[ -n "''${TMUX:-}" && -n "''${TMUX_PANE:-}" && -t 0 && -t 1 && "''${AGY_CLI_OSC7501:-}" != off ]]; then
      seen=$(${tmux} display -p -t "$TMUX_PANE" '#{pane_tty} #{pane_pipe} #{pane_pipe_pid}' 2>/dev/null)
      read -r tty piped pipe_pid <<<"$seen"
      if [[ "$tty" == "$(${pkgs.coreutils}/bin/tty)" ]] && free; then
        # tmux does not answer the CLI's check for support.
        export AGY_CLI_OSC7501=on
        # Take the pipe only if it is still as seen, and with it the state,
        # dropping what an earlier agent left.
        ${tmux} if -t "$TMUX_PANE" -F "#{==:#{pane_tty} #{pane_pipe} #{pane_pipe_pid},$seen}" "
          set -pqu -t $TMUX_PANE @agent_state
          set -p -t $TMUX_PANE @agent_state_owner $$
          pipe-pane -t $TMUX_PANE \"exec ${watcher} ${shared} ${tmux} #{q:socket_path} #{q:pane_id} $$\"
        "
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
