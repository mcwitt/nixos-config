{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.harnesses;
  configFile = ".codex/config.toml";
  envKeyFiles = config.harnesses.codex.envKeyFiles;

  # Publish each session's state to tmux; see ./tmux-state.nix.
  tmuxState = import ./tmux-state.nix { inherit config lib pkgs; };

  # Codex only runs user hooks whose definition hash is recorded as trusted
  # in [hooks.state], keyed by source file, event, group and handler index.
  # Record every hook in the final settings, whichever module added it, with
  # the hash Codex computes (codex-rs/hooks/src/engine/discovery.rs): sha256
  # of the canonical JSON of the group reduced to that one handler, with
  # defaults filled in. Declared hooks then run without a manual /hooks
  # review, and stay trusted when the store paths in their commands change.
  # PreToolUse -> pre_tool_use
  eventKey =
    event:
    lib.removePrefix "_" (
      lib.concatMapStrings (part: if lib.isList part then "_" + lib.toLower (lib.head part) else part) (
        builtins.split "([A-Z])" event
      )
    );
  hookTrust =
    event: groupIndex: group: handlerIndex: handler:
    let
      key = eventKey event;
      shortTimeout = builtins.elem event [
        "SessionEnd"
        "Interrupt"
      ];
      timeout =
        if shortTimeout then
          lib.max 1 (lib.min 3 (handler.timeout or 1))
        else
          lib.max 1 (handler.timeout or 600);
      # These events ignore the matcher.
      ignoresMatcher = builtins.elem event [
        "UserPromptSubmit"
        "Stop"
        "Interrupt"
      ];
      identity =
        removeAttrs group [ "matcher" ]
        // lib.optionalAttrs (!ignoresMatcher && group ? matcher) { inherit (group) matcher; }
        // {
          event_name = key;
          hooks = [
            (
              handler
              // {
                inherit timeout;
                async = handler.async or false;
              }
            )
          ];
        };
    in
    lib.nameValuePair
      "${config.home.homeDirectory}/${configFile}:${key}:${toString groupIndex}:${toString handlerIndex}"
      {
        trusted_hash = "sha256:" + builtins.hashString "sha256" (builtins.toJSON identity);
      };
  hookStates =
    hooks:
    lib.listToAttrs (
      lib.concatLists (
        lib.mapAttrsToList (
          event: groups:
          lib.concatLists (lib.imap0 (i: group: lib.imap0 (hookTrust event i group) group.hooks) groups)
        ) hooks
      )
    );

  # Export each envKeyFiles variable, read from its file at runtime, so
  # provider profiles can authenticate via `model_providers.<name>.env_key`.
  #
  # Inside tmux, clear the pane's state when an interactive run exits. A run
  # without a terminal, such as another agent's `codex exec`, must not report
  # on the pane of the agent that started it.
  codex = lib.getExe' pkgs.codex "codex";
  launcher = pkgs.writeShellScript "codex" ''
    ${lib.concatStrings (
      lib.mapAttrsToList (env: file: "export ${env}=\"$(cat ${lib.escapeShellArg file})\"\n") envKeyFiles
    )}
    if [[ -n "''${TMUX_PANE:-}" ]]; then
      if [[ -t 0 && -t 1 ]]; then
        ${codex} "$@"
        status=$?
        ${tmuxState.script} off </dev/null
        exit $status
      fi
      unset TMUX_PANE
    fi
    exec ${codex} "$@"
  '';

  # The generated config plus the hook trust records, which cannot live in
  # programs.codex.settings because they are computed from it.
  configSource = pkgs.runCommand "codex-config.toml" { } ''
    cat ${config.home.file.${configFile}.source} > $out
    echo >> $out
    cat ${
      (pkgs.formats.toml { }).generate "codex-hook-trust" {
        hooks.state = hookStates config.programs.codex.settings.hooks;
      }
    } >> $out
  '';
in
{
  options.harnesses.codex.envKeyFiles = lib.mkOption {
    type = lib.types.attrsOf lib.types.str;
    default = { };
    example = {
      OPENROUTER_API_KEY = "/run/agenix/openrouter-api-key";
    };
    description = ''
      Map of environment variable name to a file containing its value. The
      codex launcher exports each variable (read from its file at runtime)
      before running codex, so provider profiles can authenticate via
      `model_providers.<name>.env_key`.
    '';
  };

  config = lib.mkIf cfg.enable {
    programs.codex = {
      enable = true;
      enableMcpIntegration = true;

      settings = {
        model_reasoning_effort = "high";
        approval_policy = "on-request";
        approvals_reviewer = "auto_review";
        default_permissions = ":workspace";
        notice.hide_rate_limit_model_nudge = true;

        inherit (tmuxState) hooks;

        # Run each session in its own process. The shared background server
        # is a separately downloaded, self-updating Codex outside Nix, and it
        # runs hooks with the environment of whichever session started it,
        # so tmux state would land on that session's pane. This only stops
        # Codex from starting the server: while one started explicitly (for
        # example by `codex app-server daemon start`) runs, sessions join it.
        features.daemon_auto_start = false;

        tui = {
          vim_mode_default = true;
          status_line = [
            "thread-title"
            "model-with-reasoning"
            "current-dir"
            "context-used"
            "five-hour-limit"
            "weekly-limit"
          ];
        };
      };

      inherit (cfg) skills;
    };

    programs.codex.package = pkgs.symlinkJoin {
      name = "codex-${pkgs.codex.version}";
      inherit (pkgs.codex) version;
      paths = [ pkgs.codex ];
      postBuild = ''
        rm $out/bin/codex
        ln -s ${launcher} $out/bin/codex
      '';
      meta.mainProgram = "codex";
    };

    # release-26.05 still needs a writable config because Codex persists to it.
    # Replace this with programs.codex.mutableSettings = true when adopting
    # a Home Manager release containing the master-only fix from PR #10000:
    # https://github.com/nix-community/home-manager/pull/10000
    home.file.${configFile}.enable = lib.mkForce false;

    home.activation.codexWritableConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      run rm -f "${config.home.homeDirectory}/${configFile}"
      run install -D -m600 ${configSource} \
        "${config.home.homeDirectory}/${configFile}"
    '';
  };
}
