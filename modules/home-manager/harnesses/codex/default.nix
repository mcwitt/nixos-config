{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.harnesses;
  superpowersSkills = lib.mapAttrs' (
    name: _: lib.nameValuePair name "${inputs.superpowers}/skills/${name}"
  ) (builtins.readDir "${inputs.superpowers}/skills");
  # Relative path home-manager would manage; we copy it writable instead.
  configFile = ".codex/config.toml";
  envKeyFiles = config.harnesses.codex.envKeyFiles;
in
{
  options.harnesses.codex.envKeyFiles = lib.mkOption {
    type = lib.types.attrsOf lib.types.str;
    default = { };
    example = {
      OPENROUTER_API_KEY = "/run/agenix/openrouter-api-key";
    };
    description = ''
      Map of environment variable name to a file containing its value. When
      non-empty, the codex package is wrapped to export each variable (read
      from its file at runtime) before exec, so provider profiles can
      authenticate via `model_providers.<name>.env_key`.
    '';
  };

  config = lib.mkIf cfg.enable {
    programs.codex = {
      enable = true;
      enableMcpIntegration = true;

      settings = {
        model = "gpt-5.6-sol";
        model_reasoning_effort = "high";
        approval_policy = "on-request";
        sandbox_mode = "workspace-write";
      };

      skills =
        superpowersSkills
        // cfg.skills
        // {
          worktrunk = "${pkgs.worktrunk.src}/skills/worktrunk";
        };
    };

    programs.codex.package = lib.mkIf (envKeyFiles != { }) (
      pkgs.symlinkJoin {
        name = "codex-${pkgs.codex.version}";
        inherit (pkgs.codex) version;
        paths = [ pkgs.codex ];
        nativeBuildInputs = [ pkgs.makeWrapper ];
        postBuild = ''
          rm $out/bin/codex
          makeWrapper ${lib.getExe' pkgs.codex "codex"} $out/bin/codex \
            ${lib.concatStringsSep " " (
              lib.mapAttrsToList (
                env: file: "--run ${lib.escapeShellArg "export ${env}=\"$(cat \"${file}\")\""}"
              ) envKeyFiles
            )}
        '';
      }
    );

    # codex (>= ~0.137) persists runtime state back into config.toml — most
    # notably per-project trust via its `config/batchWrite` RPC. A read-only
    # nix-store symlink makes every such write fail ("config/batchWrite failed
    # in TUI"), so codex can't even get past the trust prompt on startup.
    #
    # Keep the config declarative (so the generated MCP nix-store paths refresh
    # on every rebuild) but install it as a writable copy on activation rather
    # than letting home-manager symlink it. Trade-off: codex's own writes (e.g.
    # trust decisions) are reset to this declarative baseline on each switch.
    home.file.${configFile}.enable = lib.mkForce false;

    home.activation.codexWritableConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      run rm -f "${config.home.homeDirectory}/${configFile}"
      run install -D -m600 ${config.home.file.${configFile}.source} \
        "${config.home.homeDirectory}/${configFile}"
    '';
  };
}
