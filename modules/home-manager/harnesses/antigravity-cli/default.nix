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
      package = pkgs.llm-agents.antigravity-cli;
      enableMcpIntegration = true;

      # Never spend separately purchased AI credits without an explicit opt-in.
      settings.useG1Credits = false;
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
        ${lib.getExe pkgs.jq} -s '.[0] * .[1]' \
          "$target" ${config.home.file.${settingsFile}.source} > "$tmp"
      else
        cp ${config.home.file.${settingsFile}.source} "$tmp"
      fi
      run install -m600 "$tmp" "$target"
      rm -f "$tmp"
    '';
  };
}
