{ config, pkgs, ... }: {

  home.file.".direnvrc".text = ''
    use_conda() {
      source $1/etc/profile.d/conda.sh
      conda activate "$2"
    }
  '';

  programs.direnv = {
    enable = true;
    enableFishIntegration = config.programs.fish.enable;
    enableZshIntegration = config.programs.zsh.enable;
  };
}
