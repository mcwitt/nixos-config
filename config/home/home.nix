{ config, pkgs, lib, ... }:

let homeDir = builtins.getEnv "HOME";
in {
  imports =
    [ ./R.nix ./haskell.nix ./npm.nix ./python.nix ./scala.nix ./secrets.nix ];

  home.packages = with pkgs;
    [
      graphviz
      niv
      nixfmt
      nix-prefetch-git
      nix-prefetch-github
      nodePackages.prettier
      pandoc
      shellcheck
    ] ++ (with gitAndTools; [ git-annex git-crypt git-remote-gcrypt hub ])
    ++ (with mypkgs; [ scripts ]);

  home.file.".emacs.d" = {
    source = "${pkgs.mypkgs.dotfiles}/emacs.d/";
    recursive = true;
  };

  home.sessionVariables = {
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
  home.stateVersion = "19.09";

  nixpkgs = {
    config.allowUnfree = true;
    overlays = (import ../utils.nix).importOverlaysDir ../../overlays;
  };

  programs.browserpass.enable = true;

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.git = {
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
    extraConfig = {
      gitHub.user = "mcwitt";
      pull.rebase = true;
    };
    delta.enable = true;
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.neovim = {
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

  programs.password-store = {
    enable = true;
    settings = { PASSWORD_STORE_DIR = "${homeDir}/.password-store/"; };
  };

  programs.readline = {
    enable = true;
    extraConfig = ''
      set editing-mode vi
      set keymap vi
    '';
  };

  programs.vscode = {
    enable = true;
    extensions = with pkgs.vscode-extensions; [
      alanz.vscode-hie-server
      justusadam.language-haskell
      vscodevim.vim
    ];
    userSettings = {
      update.mode = "none";
      extensions.autoUpdate = false;
      languageServerHaskell = {
        hieVariant = "ghcide";
        formattingProvider = "ormolu";
      };
      vim = {
        hlsearch = true;
        insertModeKeyBindings = [{
          before = [ "f" "d" ];
          after = [ "<Esc>" ];
        }];
      };
    };
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
      l = "${pkgs.coreutils}/bin/ls --color=auto -alh";
      ll = "${pkgs.coreutils}/bin/ls --color=auto -l";
      ls = "${pkgs.coreutils}/bin/ls --color=auto";
      rm = "${pkgs.coreutils}/bin/rm -i";
      git = "${pkgs.gitAndTools.hub}/bin/hub";
      g = "${pkgs.gitAndTools.hub}/bin/hub";
      ga = "${pkgs.gitAndTools.git-annex}/bin/git-annex";
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
}
