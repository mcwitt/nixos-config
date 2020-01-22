{ config, pkgs, ... }:

let homeDir = builtins.getEnv "HOME";
in {
  nixpkgs.config.allowUnfree = true;

  home.packages = with pkgs;
    [ gitAndTools.git-sync
      stack
      pipenv
    ];

  programs.urxvt = {
    enable = true;
    fonts = [ "xft:Source Code Pro:size=10" ];
    scroll.bar.enable = false;
  };

  programs.password-store = {
    enable = true;
    settings = { PASSWORD_STORE_DIR = "${homeDir}/.password-store/"; };
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.zsh = {
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
      ls = "ls --color=auto";
      rm = "rm -i";

      gl = "git l";
      gw = "git w";
    };

    sessionVariables = {
      EDITOR = pkgs.neovim;
      ALTERNATE_EDITOR = pkgs.vim;
    };

    envExtra = ''
      eval "$(${pkgs.direnv}/bin/direnv hook zsh)"
    '';

    initExtra = ''
      setopt HIST_IGNORE_SPACE
      source ${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
      source ${pkgs.zsh-history-substring-search}/share/zsh-history-substring-search/zsh-history-substring-search.zsh
      bindkey fd vi-cmd-mode
      bindkey -M vicmd 'k' history-substring-search-up
      bindkey -M vicmd 'j' history-substring-search-down
    '';
  };

  programs.git = {
    enable = true;
    userName = "Matt Wittmann";
    userEmail = "mcwitt@gmail.com";
    aliases = {
      ca = "commit --amend";
      co = "checkout";
      ri = "rebase --interactive";
      su = "submodule update --init --recursive";
      w  = "status -sb";
      l  = "log --graph --pretty=format:'%Cred%h%Creset"
         + " —%Cblue%d%Creset %s %Cgreen(%cr)%Creset'"
         + " --abbrev-commit --date=relative --show-notes=*";
    };
  };

  programs.neovim = {
    enable = true;
    vimAlias = true;
    withPython3 = true;
    plugins = with pkgs.vimPlugins;
      [ ctrlp
        syntastic
        tagbar
        vim-airline
        vim-fugitive
        vim-gitgutter
        vim-surround
        youcompleteme
      ];
    extraConfig = ''
      set expandtab
      set shiftwidth=2
      set softtabstop=2
      set nojoinspaces

      " fd returns to normal mode
      inoremap fd <esc>

      " { Syntastic
      set statusline+=%#warningmsg#
      set statusline+=%{SyntasticStatuslineFlag()}
      set statusline+=%*

      let g:syntastic_always_populate_loc_list = 1
      let g:syntastic_auto_loc_list = 1
      let g:syntastic_check_on_open = 1
      let g:syntastic_check_on_wq = 0
      " }
    '';
  };

# Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

# This value determines the Home Manager release that your
# configuration is compatible with. This helps avoid breakage
# when a new Home Manager release introduces backwards
# incompatible changes.
#
# You can update Home Manager without changing this value. See
# the Home Manager release notes for a list of state version
# changes in each release.
  home.stateVersion = "19.09";
}
