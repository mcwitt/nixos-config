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
        ${lib.optionalString (
          piCfg.extraArgs != [ ]
        ) "--add-flags ${lib.escapeShellArg (lib.escapeShellArgs piCfg.extraArgs)}"}
    '';
  };

  needsWrapper = piCfg.envKeyFiles != { } || piCfg.extraArgs != [ ];
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
      }
      (lib.mkIf (piCfg.modelsJson != { }) {
        home.file.".pi/agent/models.json".source = jsonFormat.generate "pi-models.json" piCfg.modelsJson;
      })
    ]
  );
}
