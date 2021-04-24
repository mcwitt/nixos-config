{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.tools.kubernetes;
  sources = import ../../../../../nix/sources.nix;
in
{
  options.tools.kubernetes.enable =
    mkEnableOption "Kubernetes command-line tools";

  config = mkIf cfg.enable {
    home.packages = [ pkgs.kubectl ];

    shells.aliases.k = "${pkgs.kubectl}/bin/kubectl";

    xdg.configFile."fish/completions/kubectl.fish".source = mkIf config.programs.fish.enable
      "${sources.fish-kubectl-completions}/completions/kubectl.fish";
  };
}
