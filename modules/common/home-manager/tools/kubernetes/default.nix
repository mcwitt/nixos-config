{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.tools.kubernetes;
in
{
  options.tools.kubernetes.enable = mkEnableOption "Kubernetes command-line tools";

  config = mkIf cfg.enable {
    home.packages = [ pkgs.kubectl ];
    home.shellAliases.k = "${pkgs.kubectl}/bin/kubectl";

    programs.fish.interactiveShellInit = mkIf config.programs.fish.enable ''
      ${pkgs.kubectl}/bin/kubectl completion fish | source
    '';
  };
}
