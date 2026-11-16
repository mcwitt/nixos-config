{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.harnesses;
  piCfg = config.harnesses.pi;
  jsonFormat = pkgs.formats.json { };

  # Sourced from the numtide/llm-agents.nix overlay (already applied in
  # flake.nix) rather than nixpkgs: it tracks pi's near-daily releases, whereas
  # nixpkgs lags by weeks. Bump with `nix flake update llm-agents`.
  piPkg = pkgs.llm-agents.pi;

  piPackages = import ./packages.nix { inherit pkgs; };

  # Settings the declarative config owns, merged into the writable
  # settings.json by the activation below. Store-path packages are replaced on
  # each switch; packages added with `pi install` are kept.
  managedSettings = piCfg.settings // {
    packages = map toString piCfg.packages;
  };

  env =
    piCfg.env
    // lib.optionalAttrs (piCfg.autoMode != { }) {
      PI_AUTOMODE_SETTINGS_JSON = builtins.toJSON piCfg.autoMode;
    };

  # The pinned home-manager has no pi module, so we wire it by
  # hand: install the package, optionally wrapped to export provider API keys
  # from files at runtime and to pin a default provider/model.
  wrapped = pkgs.symlinkJoin {
    name = "pi-${piPkg.version}";
    inherit (piPkg) version;
    paths = [ piPkg ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      rm $out/bin/pi
      makeWrapper ${lib.getExe' piPkg "pi"} $out/bin/pi \
        ${
          lib.concatStringsSep " " (
            lib.mapAttrsToList (
              env: file: "--run ${lib.escapeShellArg "export ${env}=\"$(cat \"${file}\")\""}"
            ) piCfg.envKeyFiles
          )
        } \
        ${
          lib.concatStringsSep " " (
            lib.mapAttrsToList (name: value: "--set ${lib.escapeShellArg name} ${lib.escapeShellArg value}") env
          )
        } \
        ${lib.optionalString (
          piCfg.extraArgs != [ ]
        ) "--add-flags ${lib.escapeShellArg (lib.escapeShellArgs piCfg.extraArgs)}"}
    '';
  };

  needsWrapper = piCfg.envKeyFiles != { } || env != { } || piCfg.extraArgs != [ ];
  package = if needsWrapper then wrapped else piPkg;

  # pi-acp spawns `pi --mode rpc` via a bare PATH lookup (see
  # src/pi-rpc/command.ts in svkozak/pi-acp). Emacs launched from the GUI
  # doesn't inherit `~/.nix-profile/bin` on exec-path, so the spawn fails
  # with ENOENT and prompts silently go nowhere. Wrap the adapter to prepend
  # the final pi binary's bin dir to PATH, so it resolves regardless of the
  # Emacs process environment.
  acpPackage = pkgs.symlinkJoin {
    name = "pi-acp-wrapped";
    paths = [ pkgs.pi-acp ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/pi-acp --prefix PATH : ${lib.makeBinPath [ package ]}
    '';
  };

in
{
  imports = [ ./theme.nix ];

  options.harnesses.pi = {
    envKeyFiles = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      example = {
        OPENROUTER_API_KEY = "/run/agenix/openrouter-api-key";
      };
      description = ''
        Map of environment variable name to a file containing its value. When
        non-empty, the pi package is wrapped to export each variable (read from
        its file at runtime) before exec, so providers can authenticate without
        an interactive `/login` (pi resolves API keys from the environment).
      '';
    };

    env = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      description = ''
        Environment variables the pi wrapper sets, for values that are not
        secret. Use {option}`envKeyFiles` for secrets.
      '';
    };

    settings = lib.mkOption {
      type = jsonFormat.type;
      default = { };
      example = {
        defaultThinkingLevel = "high";
      };
      description = ''
        Settings merged into `~/.pi/agent/settings.json` on activation. pi
        rewrites that file at runtime (`/settings`, `/model`, `pi install`), so
        it stays writable: these keys are reset to the declared values on each
        switch, and every other key is kept.
      '';
    };

    packages = lib.mkOption {
      type = lib.types.listOf lib.types.path;
      default = [ ];
      description = ''
        pi packages loaded from local store paths (the `packages` setting).
        Each is a directory whose package.json `pi` manifest names its
        extensions and skills. Packages that `pi install` adds at runtime are
        kept beside these.
      '';
    };

    autoMode = lib.mkOption {
      type = jsonFormat.type;
      default = { };
      description = ''
        pi-automode configuration, passed through `PI_AUTOMODE_SETTINGS_JSON`.
        That source outranks `~/.pi/agent/extensions/pi-automode/config.json`
        and a trusted project's `.pi/automode.local.json`, so keys set here
        cannot be changed at runtime (for example by `/automode model`); keys
        left out still can. Definitions merge, so downstream flakes can add
        `autoMode.environment` entries.
      '';
    };

    extraArgs = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      example = [
        "--provider"
        "openrouter"
        "--model"
        "deepseek/deepseek-v4-pro"
      ];
      description = ''
        Flags appended to every `pi` invocation via the wrapper, e.g. to pin a
        default provider/model so the first launch lands on a configured model
        rather than pi's built-in default (which may have no key).
      '';
    };

    modelsJson = lib.mkOption {
      type = jsonFormat.type;
      default = { };
      description = ''
        Contents of `~/.pi/agent/models.json` (custom providers/models). When
        non-empty it is written declaratively; pi reloads it when you open
        `/model`. Use to surface provider slugs pi does not ship built-in. pi
        only reads this file (it persists credentials to auth.json instead), so
        a read-only nix-store symlink is safe. Definitions from separate modules
        merge recursively, so each provider can be declared where it is wired.
      '';
    };

    extensions = lib.mkOption {
      type = lib.types.attrsOf lib.types.path;
      default = { };
      example = lib.literalExpression ''
        {
          "my-extension.ts" = ./my-extension.ts;
          my-package = ./my-package; # a directory containing index.ts
        }
      '';
      description = ''
        Extensions installed under `~/.pi/agent/extensions`, keyed by entry
        name. pi loads each TypeScript or JavaScript file there, and each
        subdirectory containing an `index.ts` or `index.js`, at startup.
        Definitions merge, so downstream flakes can add their own.
      '';
    };

    finalPackage = lib.mkOption {
      type = lib.types.package;
      readOnly = true;
      description = ''
        The final `pi` package, wrapped per {option}`envKeyFiles` and
        {option}`extraArgs` when applicable. Consumers that spawn `pi` as a
        subprocess (e.g. `pi-acp`) should use this rather than
        `pkgs.llm-agents.pi` so they resolve the same env-injected binary.
      '';
    };

    acpPackage = lib.mkOption {
      type = lib.types.package;
      readOnly = true;
      description = ''
        `pkgs.pi-acp` wrapped to find {option}`finalPackage` on its `PATH`,
        so the adapter can spawn `pi --mode rpc` without relying on the
        user-level profile being on Emacs's `exec-path`.
      '';
    };
  };

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      {
        home.packages = [ package ];
        harnesses.pi.finalPackage = package;
        harnesses.pi.acpPackage = acpPackage;

        harnesses.pi = {
          packages = [
            piPackages.claude-code
            piPackages.automode
          ];

          # The unwrapped binary: the wrapper on PATH adds --plugin-dir flags,
          # and worktrunk's plugin hooks would then fire from inside pi's pane.
          # pi-claude-code already passes --setting-sources "" so the user
          # settings (and their hooks) stay out.
          env.PI_CLAUDE_CODE_COMMAND = lib.getExe config.programs.claude-code.package;

          settings = {
            # Subscriptions only: Claude through pi-claude-code, OpenAI through
            # the ChatGPT login (/login → openai-codex). No API-key provider
            # is enabled; keep ANTHROPIC_API_KEY and OPENAI_API_KEY out of
            # envKeyFiles.
            defaultProvider = lib.mkDefault "claude-code";
            defaultModel = lib.mkDefault "claude-opus-5-5";
            defaultThinkingLevel = lib.mkDefault "high";
            enabledModels = [
              "claude-code/*"
              "openai-codex/*"
            ];

            # Ctrl+g: always a terminal frame, as for Claude Code;
            # emacsclient-auto's display sniffing misfires inside a multiplexer
            # attached over SSH.
            externalEditor = "emacsclient -t";
          };

          # pi-automode: every tool call that no deterministic rule settles goes
          # to a classifier, as in Claude Code's auto mode. The classifier runs
          # on the ChatGPT subscription; without a fixed model it would use the
          # session model, which for Claude means one `claude -p` per call.
          autoMode.autoMode = {
            classifierModel = lib.mkDefault "openai-codex/gpt-6-luna";
            classifierReasoningLevel = lib.mkDefault "low";
            environment = [
              "$defaults"
              "Source control: github.com/mcwitt and all repositories under it are the user's own."
            ];
          };
        };

        # Publish each session's state to tmux; see ./tmux-state.ts.
        harnesses.pi.extensions."tmux-state.ts" = pkgs.replaceVars ./tmux-state.ts {
          agentTmuxState = import ../tmux-state.nix { inherit config pkgs; };
        };

        # pi has no skills option either; it discovers skills by scanning
        # ~/.pi/agent/skills for SKILL.md at startup.
        home.file =
          lib.mapAttrs' (
            name: source: lib.nameValuePair ".pi/agent/skills/${name}" { inherit source; }
          ) cfg.skills
          // lib.mapAttrs' (
            name: source: lib.nameValuePair ".pi/agent/extensions/${name}" { inherit source; }
          ) piCfg.extensions;

        # The pinned home-manager has no pi module, and pi rewrites
        # settings.json at runtime, so merge the declared settings into a
        # writable file rather than linking it from the store.
        home.activation.piSettings = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          PI_SETTINGS="${config.home.homeDirectory}/.pi/agent/settings.json"
          run mkdir -p "$(dirname "$PI_SETTINGS")"
          if [ -e "$PI_SETTINGS" ]; then
            _pi_current="$PI_SETTINGS"
          else
            _pi_current=${pkgs.writeText "empty.json" "{}"}
          fi
          _pi_settings="$(${lib.getExe pkgs.jq} \
            --slurpfile managed ${jsonFormat.generate "pi-managed-settings.json" managedSettings} '
              $managed[0] as $m
              | (. * ($m | del(.packages)))
              | .packages = (
                  [(.packages // [])[]
                    | select(((if type == "object" then .source else . end) // "")
                        | startswith("/nix/store/") | not)]
                  + $m.packages)
            ' "$_pi_current")"
          _pi_tmp="$(${lib.getExe' pkgs.coreutils "mktemp"})"
          printf '%s\n' "$_pi_settings" > "$_pi_tmp"
          run install -m644 "$_pi_tmp" "$PI_SETTINGS"
          rm -f "$_pi_tmp"
        '';
      }
      (lib.mkIf (piCfg.modelsJson != { }) {
        home.file.".pi/agent/models.json".source = jsonFormat.generate "pi-models.json" piCfg.modelsJson;
      })
    ]
  );
}
