{ config, lib, pkgs, ... }:
with lib;
let cfg = config.tools.kubernetes; in
{
  options.tools.kubernetes.enable =
    mkEnableOption "Kubernetes command-line tools";

  config = mkIf cfg.enable {
    home.packages = [ pkgs.kubectl ];
    home.shellAliases.k = "${pkgs.kubectl}/bin/kubectl";

    xdg.configFile."fish/completions/kubectl.fish".source = mkIf config.programs.fish.enable
      "${pkgs.fish-kubectl-completions}/completions/kubectl.fish";
  };
}
