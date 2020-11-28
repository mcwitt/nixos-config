{ config, pkgs, lib, ... }:
with lib;
let cfg = config.profiles.default;
in
{
  options.profiles.default.enable = mkEnableOption "Default profile";

  config = mkIf cfg.enable {
    home.packages = with pkgs;
      [ bat cachix csvkit graphviz nixops pandoc python3Packages.sqlparse ]
      ++ (with gitAndTools; [ delta git-annex git-crypt git-remote-gcrypt hub ])
      ++ (with mypkgs; [ scripts ]);

    home.sessionVariables.EDITOR =
      "${config.programs.emacs.finalPackage}/bin/emacs";
    home.sessionVariables.ALTERNATE_EDITOR = "${pkgs.vim}/bin/vim";

    home.stateVersion = "20.09";

    nixpkgs.config = {
      allowBroken = true;
      allowUnfree = true;
    };

    nixpkgs.overlays = import ../../../overlays;

    programs.browserpass.enable = true;

    programs.direnv = {
      enable = true;
      enableBashIntegration = true;
      enableFishIntegration = true;
      enableNixDirenvIntegration = true;
      enableZshIntegration = true;
    };

    programs.emacs = {
      enable = true;
      baseConfig.enable = true;
      package = pkgs.emacsGit;
    };

    programs.fish = {
      enable = true;
      shellAliases.cdr =
        lib.mkForce "cd (${pkgs.git}/bin/git rev-parse --show-toplevel)";

      functions = {
        fish_user_key_bindings = ''
          fish_vi_key_bindings
          bind -M insert -m default fd backward-char force-repaint
        '';
        gitignore = "curl -sL https://www.gitignore.io/api/$argv";
      };

      interactiveShellInit = ''
        set fish_key_bindings fish_user_key_bindings
      '';
    };

    programs.fzf = {
      enable = true;
      enableFishIntegration = true;
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
        exec = "!exec ";
        ri = "rebase --interactive";
        su = "submodule update --init --recursive";
        w = "status -sb";
        l = "log --graph --pretty=format:'%Cred%h%Creset"
          + " —%Cblue%d%Creset %s %Cgreen(%cr)%Creset'"
          + " --abbrev-commit --date=relative --show-notes=*";
      };
      ignores = lib.concatMap pkgs.mypkgs.gitignore.ghGitIgnoreLines [
        "Global/Vim"
        "Global/Emacs"
      ];
      signing = {
        key = "A79A94078DF3DB5B";
        signByDefault = true;
      };
      extraConfig = {
        gitHub.user = "mcwitt";
        merge.conflictStyle = "diff3";
        pull.rebase = true;
      };
      delta.enable = true;
    };

    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;

    programs.mpv.enable = true;

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
      package =
        pkgs.pass.withExtensions (exts: with exts; [ pass-update pass-otp ]);
      settings = {
        PASSWORD_STORE_DIR = "${config.home.homeDirectory}/.password-store/";
      };
    };

    programs.readline = {
      enable = true;
      extraConfig = ''
        set editing-mode vi
        set keymap vi
      '';
    };

    programs.starship = {
      enable = true;
      enableFishIntegration = true;
      enableZshIntegration = true;
      settings.directory.fish_style_pwd_dir_length = 1;
    };

    programs.tmux = {
      enable = true;
      keyMode = "vi";
    };

    programs.vscode = {
      enable = true;
      extensions = [ pkgs.vscode-extensions.vscodevim.vim ];
      userSettings = {
        update.mode = "none";
        extensions.autoUpdate = false;
        workbench.colorTheme = "Solarized Dark";
        editor = {
          fontFamily = "'Fira Code'";
          fontLigatures = true;
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
      autocd = true;
      defaultKeymap = "viins";
      enableAutosuggestions = true;
      enableCompletion = false;

      history = {
        size = 50000;
        save = 500000;
        ignoreDups = true;
        extended = true;
      };

      initExtra = ''
        setopt HIST_IGNORE_SPACE
        source ${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
        source ${pkgs.zsh-history-substring-search}/share/zsh-history-substring-search/zsh-history-substring-search.zsh
        PROMPT='%B%(?..[%?] )%b%n@%U%m%u$(git_super_status) %# '
        RPROMPT='%F{green}%~%f'
        bindkey fd vi-cmd-mode
        bindkey -M vicmd 'k' history-substring-search-up
        bindkey -M vicmd 'j' history-substring-search-down
      '';
    };

    languages = {
      R.enable = true;
      dhall.enable = true;
      haskell.enable = true;
      js.enable = true;
      nix.enable = true;
      python.enable = true;
      rust.enable = true;
      scala.enable = true;
      shell.enable = true;
    };

    shell.aliases = {
      cdr = ''cd "$(${pkgs.git}/bin/git rev-parse --show-toplevel)"'';
      l = "${pkgs.coreutils}/bin/ls --color=auto -alh";
      ll = "${pkgs.coreutils}/bin/ls --color=auto -l";
      ls = "${pkgs.coreutils}/bin/ls --color=auto";
      git = "${pkgs.gitAndTools.hub}/bin/hub";
      g = "${pkgs.gitAndTools.hub}/bin/hub";
      ga = "${pkgs.gitAndTools.git-annex}/bin/git-annex";
      gb = "${pkgs.git}/bin/git b";
      gca = "${pkgs.git}/bin/git ca";
      gd = "${pkgs.git}/bin/git d";
      gds = "${pkgs.git}/bin/git ds";
      gl = "${pkgs.git}/bin/git l";
      gw = "${pkgs.git}/bin/git w";
      rm = "${pkgs.coreutils}/bin/rm -i";
    };

    tools = {
      aws.enable = true;
      kubernetes.enable = true;
    };
  };
}
