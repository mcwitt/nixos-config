{ config, pkgs, ... }:

{
  home.stateVersion = "19.09";

  programs = {
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

    neovim = {
      enable = true;
      vimAlias = true;
      withPython3 = true;
      plugins = with pkgs.vimPlugins; [
        ctrlp
        syntastic
        tagbar
        vim-airline
        vim-colors-solarized
        vim-fugitive
        vim-gitgutter
        vim-surround
        youcompleteme
      ];
      extraConfig = ''
        set expandtab
        set nojoinspaces
        set background=dark

        " fd returns to normal mode
        inoremap fd <esc>

        colorscheme solarized

        " { Syntastic
        set statusline+=%#warningmsg#
        set statusline+=%{SyntasticStatuslineFlag()}
        set statusline+=%*

        let g:syntastic_always_populate_loc_list = 1
        let g:syntastic_auto_loc_list = 1
        let g:syntastic_check_on_open = 1
        let g:syntastic_check_on_wq = 0
        " } Syntastic
      '';
    };

    home-manager.enable = true;
  };
}
