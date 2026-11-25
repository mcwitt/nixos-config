{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.harnesses;
  settingsFile = ".gemini/antigravity-cli/settings.json";
in
{
  config = lib.mkIf cfg.enable {
    programs.antigravity-cli = {
      enable = true;
      package = import ./tmux-state.nix {
        inherit config pkgs;
        package = pkgs.llm-agents.antigravity-cli;
      };
      enableMcpIntegration = true;

      settings = {
        agentMode = "auto";
        # The terminal sandbox needs the Nix dynamic linker and the tools it
        # loads. Home Manager's skill symlinks also resolve into the store.
        permissions.allow = [ "read_file(${builtins.storeDir})" ];
        # Never spend separately purchased AI credits without an explicit opt-in.
        useG1Credits = false;
      };
    };

    # home-manager release-26.05 still uses the former
    # ~/.gemini/config/skills path. Current Antigravity CLI releases discover
    # their global skills here; this can move back to the upstream `skills`
    # option once an equivalent fix to home-manager commit 34dd288e reaches the
    # release branch.
    home.file =
      lib.mapAttrs' (
        name: source:
        lib.nameValuePair ".gemini/antigravity-cli/skills/${name}" {
          inherit source;
        }
      ) cfg.skills
      // {
        ${settingsFile}.enable = lib.mkForce false;
      };

    # Antigravity persists changes made through /config to settings.json. Merge
    # the declarative keys into a writable copy so those runtime preferences
    # survive subsequent home-manager activations.
    home.activation.antigravityWritableSettings = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      target="${config.home.homeDirectory}/${settingsFile}"
      run mkdir -p "$(dirname "$target")"
      tmp="$(${lib.getExe' pkgs.coreutils "mktemp"})"
      if [ -e "$target" ]; then
        # Object multiplication replaces arrays. Keep grants saved by /permissions.
        ${lib.getExe pkgs.jq} -s '
          .[0] as $saved | .[1] as $managed | ($saved * $managed)
          | .permissions.allow = (($saved.permissions.allow // []) + $managed.permissions.allow | unique)
        ' \
          "$target" ${config.home.file.${settingsFile}.source} > "$tmp"
      else
        cp ${config.home.file.${settingsFile}.source} "$tmp"
      fi
      run install -m600 "$tmp" "$target"
      rm -f "$tmp"
    '';
  };
}
