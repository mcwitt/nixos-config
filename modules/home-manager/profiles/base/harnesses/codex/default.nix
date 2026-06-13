{
  config,
  gwsSkills,
  localSkills,
  superpowersSkills,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.profiles.base;
  # Relative path home-manager would manage; we copy it writable instead.
  configFile = ".codex/config.toml";
in
{
  config = lib.mkIf cfg.enable {
    programs.codex = {
      enable = true;
      enableMcpIntegration = true;

      settings = {
        model = "gpt-5.5";
        model_reasoning_effort = "high";
        approval_policy = "on-request";
        sandbox_mode = "workspace-write";
      };

      skills =
        superpowersSkills
        // gwsSkills
        // localSkills
        // {
          worktrunk = "${pkgs.worktrunk.src}/skills/worktrunk";
        };
    };

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
