{ config, pkgs, lib, ... }:

let
  homeDir = builtins.getEnv "HOME";
  passwordStoreDir = "${homeDir}/.password-store/";
in {
  imports = [ ./emacs.nix ];
  home = {
    packages = with pkgs;
      [ pandoc pipenv scripts shellcheck stack ]
      ++ (with pkgs.gitAndTools; [ git-crypt git-sync hub ]);

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

    overlays = let path = ../../overlays;
    in with builtins;
    map (n: import (path + ("/" + n))) (filter (n:
      match ".*\\.nix" n != null
      || pathExists (path + ("/" + n + "/default.nix")))
      (attrNames (readDir path)));
  };

  programs = {
    browserpass.enable = true;

    direnv = {
      enable = true;
      enableZshIntegration = true;
    };

    emacs.enable = true;

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
        ri = "rebase --interactive";
        su = "submodule update --init --recursive";
        w = "status -sb";
        l = "log --graph --pretty=format:'%Cred%h%Creset"
          + " —%Cblue%d%Creset %s %Cgreen(%cr)%Creset'"
          + " --abbrev-commit --date=relative --show-notes=*";
      };
      ignores =
        lib.concatMap pkgs.ghGitIgnoreLines [ "Global/Emacs" "Global/Vim" ];
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

    password-store = {
      enable = true;
      package =
        pkgs.pass.withExtensions (exts: with exts; [ pass-otp pass-update ]);
      settings = { PASSWORD_STORE_DIR = passwordStoreDir; };
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
        gb = "${pkgs.git}/bin/git b";
        gl = "${pkgs.git}/bin/git l";
        gw = "${pkgs.git}/bin/git w";
        git = "${pkgs.gitAndTools.hub}/bin/hub";
        clip = "${pkgs.scripts}/bin/pass-clip";
      };

      sessionVariables = {
        EDITOR = "${config.programs.neovim.finalPackage}/bin/nvim";
        ALTERNATE_EDITOR = "${pkgs.vim}/bin/vim";
        ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE = "fg=10"; # fix invisible hints
      };

      initExtra = ''
        setopt HIST_IGNORE_SPACE
        source ${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
        source ${pkgs.zsh-history-substring-search}/share/zsh-history-substring-search/zsh-history-substring-search.zsh
        source ${pkgs.zsh-git-prompt}/share/zsh-git-prompt/zshrc.sh
        PROMPT='%B%(?..[%?] )%b%n@%U%m%u$(git_super_status) %# '
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
