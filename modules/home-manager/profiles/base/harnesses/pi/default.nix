{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.profiles.base;
  piCfg = config.harnesses.pi;

  # Sourced from the numtide/llm-agents.nix overlay (already applied in
  # flake.nix) rather than nixpkgs: it tracks pi's near-daily releases, whereas
  # nixpkgs lags by weeks. Bump with `nix flake update llm-agents`.
  piPkg = pkgs.llm-agents.pi;

  # pi has no home-manager module (only the nixpkgs package), so we wire it by
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
        ) "--add-flags ${lib.escapeShellArg (lib.concatStringsSep " " piCfg.extraArgs)}"}
    '';
  };

  needsWrapper = piCfg.envKeyFiles != { } || piCfg.extraArgs != [ ];
  package = if needsWrapper then wrapped else piPkg;
in
{
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
      type = lib.types.attrs;
      default = { };
      description = ''
        Contents of `~/.pi/agent/models.json` (custom providers/models). When
        non-empty it is written declaratively; pi reloads it when you open
        `/model`. Use to surface provider slugs pi does not ship built-in. pi
        only reads this file (it persists credentials to auth.json instead), so
        a read-only nix-store symlink is safe.
      '';
    };
  };

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      {
        home.packages = [ package ];
      }
      (lib.mkIf (piCfg.modelsJson != { }) {
        home.file.".pi/agent/models.json".source =
          (pkgs.formats.json { }).generate "pi-models.json"
            piCfg.modelsJson;
      })
    ]
  );
}
