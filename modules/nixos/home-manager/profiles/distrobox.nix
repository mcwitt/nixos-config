{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.profiles.distrobox;
in
{
  options.profiles.distrobox.enable = lib.mkEnableOption "Enable distrobox";

  config = lib.mkIf cfg.enable {

    programs = {

      # Override home-manager's default direnv shell integrations to skip if we're in a distrobox container
      direnv = {
        enableBashIntegration = false;
        # enableFishIntegration = false; NOTE: read-only, set to true
        enableZshIntegration = false;
      };

      bash.initExtra = lib.mkAfter ''
        if [ -z "''${CONTAINER_ID:-}" ]; then
            eval "$(${lib.getExe pkgs.direnv} hook bash)"
        fi
      '';

      # NOTE: since fish integration can't be disabled in
      # home-manager, erase the hook at the end of init
      # https://github.com/direnv/direnv/issues/550#issuecomment-933579968
      fish.interactiveShellInit = lib.mkOrder 2000 ''
        if set -q CONTAINER_ID
            functions --erase __direnv_export_eval && functions --erase __direnv_export_eval_2
        end
      '';

      zsh.initContent = lib.mkAfter ''
        if [ -z "''${CONTAINER_ID:-}" ]; then
            eval "$(${lib.getExe pkgs.direnv} hook zsh)"
        fi
      '';

      distrobox = {
        enable = true;

        containers = {
          cuda-dev = {
            image = "nvidia/cuda:12.8.1-devel-ubuntu24.04";
            additional_flags = "--gpus all";
          };
        };
      };
    };
  };
}
