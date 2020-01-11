{ config, pkgs, ... }:

{
  home.stateVersion = "19.09";

  programs = {
    home-manager.enable = true;

    fzf = {
      enable = true;
      enableZshIntegration = true;
    };

    zsh = {
      enable = true;
      enableCompletion = false;
      enableAutosuggestions = true;

      autocd = true;
      defaultKeymap = "viins";

      history = {
        size = 50000;
        save = 500000;
        ignoreDups = true;
        extended = true;
      };

      shellAliases = {
        l = "ls -alh";
        ll = "ls -l";
        ls = "ls -G";
        rm = "rm -i";
      };

      initExtra = ''
        setopt HIST_IGNORE_SPACE

        source ${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
        source ${pkgs.zsh-history-substring-search}/share/zsh-history-substring-search/zsh-history-substring-search.zsh
        bindkey fd vi-cmd-mode
        bindkey -M vicmd 'k' history-substring-search-up
        bindkey -M vicmd 'j' history-substring-search-down
      '';
    };

    git = {
      enable = true;
      userName = "Matt Wittmann";
      userEmail = "mcwitt@gmail.com";
      aliases = {
        ca = "commit --amend";
        co = "checkout";
        ri = "rebase --interactive";
        su = "submodule update --init --recursive";
        l  = "log --graph --pretty=format:'%Cred%h%Creset"
             + " —%Cblue%d%Creset %s %Cgreen(%cr)%Creset'"
             + " --abbrev-commit --date=relative --show-notes=*";
      };
    };
  };
}
