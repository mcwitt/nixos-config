{ pkgs, ... }: {
  home.packages = [ pkgs.kubectl ];
  xdg.configFile."fish/completions/kubectl.fish".source =
    "${pkgs.mypkgs.sources.fish-kubectl-completions}/completions/kubectl.fish";
}
