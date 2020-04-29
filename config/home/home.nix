{ config, pkgs, lib, ... }:

let homeDir = builtins.getEnv "HOME";
in {
  imports = [
    ./R.nix
    ./haskell.nix
    ./scala.nix
    ./jupyterlab.nix
    ./npm.nix
    ./secrets.nix
  ];
  home = {
    packages = with pkgs;
      [
        graphviz
        nixfmt
        nix-prefetch-git
        nodePackages.prettier
        pandoc
        python37Packages.black
        shellcheck
        stack
      ] ++ (with gitAndTools; [
        git-annex
        git-crypt
        git-remote-gcrypt
        git-sync
        hub
      ]) ++ (with mypkgs; [ scripts ]);

    file.".emacs.d" = {
      source = "${pkgs.mypkgs.dotfiles}/emacs.d/";
      recursive = true;
    };

    sessionVariables = {
      EDITOR = "${pkgs.mypkgs.emacs}/bin/emacsclient --tty";
      ALTERNATE_EDITOR = "${pkgs.vim}/bin/vim";
    };

    # This value determines the Home Manager release that your
    # configuration is compatible with. This helps avoid breakage
    # when a new Home Manager release introduces backwards
    # incompatible changes.
    #
    # You can update Home Manager without changing this value. See
    # the Home Manager release notes for a list of state version
    # changes in each release.
    stateVersion = "19.09";
  };

  nixpkgs = {
    config.allowUnfree = true;
    overlays = (import ../utils.nix).importOverlaysDir ../../overlays;
  };

  programs = {
    browserpass.enable = true;

    direnv = {
      enable = true;
      enableZshIntegration = true;
    };

    fzf = {
      enable = true;
      enableZshIntegration = true;
    };

    git = {
      enable = true;
      userName = "Matt Wittmann";
      userEmail = "mcwitt@gmail.com";
      aliases = {
        b = "branch --color -v";
        ca = "commit --amend";
        co = "checkout";
        d = "diff HEAD";
        ds = "diff --staged";
        ri = "rebase --interactive";
        su = "submodule update --init --recursive";
        w = "status -sb";
        l = "log --graph --pretty=format:'%Cred%h%Creset"
          + " —%Cblue%d%Creset %s %Cgreen(%cr)%Creset'"
          + " --abbrev-commit --date=relative --show-notes=*";
      };
      ignores = lib.concatMap pkgs.mypkgs.gitignore.ghGitIgnoreLines [
        "Global/Emacs"
        "Global/Vim"
      ];
      signing = {
        key = "A79A94078DF3DB5B";
        signByDefault = true;
      };
      extraConfig.gitHub.user = "mcwitt";
    };

    # Let Home Manager install and manage itself.
    home-manager.enable = true;

    neovim = {
      enable = true;
      vimAlias = true;
      withPython3 = true;
      plugins = with pkgs.vimPlugins; [
        ctrlp
        syntastic
        tagbar
        vim-airline
        vim-fugitive
        vim-gitgutter
        vim-surround
        YouCompleteMe
      ];

      extraConfig = ''
        set expandtab
        set shiftwidth=2
        set softtabstop=2
        set nojoinspaces

        " fd returns to normal mode
        inoremap fd <esc>

        set statusline+=%#warningmsg#
        set statusline+=%{SyntasticStatuslineFlag()}
        set statusline+=%*

        let g:syntastic_always_populate_loc_list = 1
        let g:syntastic_auto_loc_list = 1
        let g:syntastic_check_on_open = 1
        let g:syntastic_check_on_wq = 0
      '';
    };

    password-store = {
      enable = true;
      settings = { PASSWORD_STORE_DIR = "${homeDir}/.password-store/"; };
    };

    readline = {
      enable = true;
      extraConfig = ''
        set editing-mode vi
        set keymap vi
      '';
    };

    urxvt = {
      enable = true;
      extraConfig = {
        # clickable URLs
        perl-ext-common = "default,matcher";
        url-launcher = "${pkgs.xdg_utils}/bin/xdg-open";
        "matcher.button" = 1;
      };
      fonts = [ "xft:Fira Code:size=10" ];
      scroll.bar.enable = false;
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
        l = "${pkgs.coreutils}/bin/ls --color=auto -alh";
        ll = "${pkgs.coreutils}/bin/ls --color=auto -l";
        ls = "${pkgs.coreutils}/bin/ls --color=auto";
        rm = "${pkgs.coreutils}/bin/rm -i";
        git = "${pkgs.gitAndTools.hub}/bin/hub";
        g = "${pkgs.gitAndTools.hub}/bin/hub";
        gb = "${pkgs.git}/bin/git b";
        gd = "${pkgs.git}/bin/git d";
        gds = "${pkgs.git}/bin/git ds";
        gl = "${pkgs.git}/bin/git l";
        gw = "${pkgs.git}/bin/git w";
        ec = "${pkgs.mypkgs.emacs}/bin/emacsclient --tty";
        emacs = "${pkgs.mypkgs.emacs}/bin/emacsclient --create-frame";
      };

      # fix invisible hints
      sessionVariables.ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE = "fg=10";

      initExtra = ''
        setopt HIST_IGNORE_SPACE
        source ${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
        source ${pkgs.zsh-history-substring-search}/share/zsh-history-substring-search/zsh-history-substring-search.zsh
        source ${pkgs.zsh-git-prompt}/share/zsh-git-prompt/zshrc.sh
        PROMPT='%B%(?..[%?] )%b%n@%U%m%u$(git_super_status) %# '
        RPROMPT='%F{green}%~%f'
        bindkey fd vi-cmd-mode
        bindkey -M vicmd 'k' history-substring-search-up
        bindkey -M vicmd 'j' history-substring-search-down
      '';
    };
  };

  xdg.enable = true;

  xresources.extraConfig = builtins.readFile (pkgs.fetchFromGitHub {
    owner = "altercation";
    repo = "solarized";
    rev = "62f656a02f93c5190a8753159e34b385588d5ff3";
    sha256 = "0001mz5v3a8zvi3gzmxhi3yrsb6hs7qf6i497arsngnvj2cwn61d";
  } + "/xresources/solarized");

  xsession.pointerCursor = {
    package = pkgs.vanilla-dmz;
    name = "Vanilla-DMZ";
    size = 48;
  };
}
