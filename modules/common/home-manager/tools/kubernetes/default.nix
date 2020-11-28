{ config, lib, pkgs, ... }:
with lib;
let cfg = config.tools.kubernetes;
in
{
  options.tools.kubernetes.enable =
    mkEnableOption "Kubernetes command-line tools";

  config = mkIf cfg.enable {
    home.packages = [ pkgs.kubectl ];
    xdg.configFile."fish/completions/kubectl.fish".source =
      "${pkgs.mypkgs.sources.fish-kubectl-completions}/completions/kubectl.fish";
  };
}
