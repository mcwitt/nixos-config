{ config, lib, pkgs, ... }:

let sources = import ../../../../../nix/sources.nix; in
{
  imports = [ ./emacs ./neovim ];

  home.packages = with pkgs;
    [
      delta
      git-annex
      git-crypt
      git-remote-gcrypt
      hub
      lab
      nix-du
      pkgs.mcwitt.scripts
      pre-commit
      proselint
      ripgrep
      yq
    ];

  home.sessionPath = [ "${config.home.homeDirectory}/.local/bin" ];

  home.sessionVariables = {
    EDITOR = "${config.programs.emacs.finalPackage}/bin/emacsclient -c";
    ALTERNATE_EDITOR = "${pkgs.vim}/bin/vim";
  };

  home.stateVersion = "21.11";

  languages.nix.enable = true;

  programs.bat.enable = true;

  programs.browserpass.enable = true;

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };

  programs.emacs.enable = true;

  programs.fish = {
    enable = true;

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

    shellAliases.cdr =
      lib.mkForce "cd (${pkgs.git}/bin/git rev-parse --show-toplevel)";
  };

  programs.fzf = {
    enable = true;
    enableFishIntegration = true;
    enableZshIntegration = true;
  };

  programs.git = {
    enable = true;

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
      l = "log --graph";
      wip = lib.concatStringsSep " " [
        "for-each-ref"
        "--sort='authordate:iso8601'"
        "--format=' %(color:green)%(authordate:relative)%09%(color:white)%(refname:short)'"
        "refs/heads"
      ];
    };

    ignores = (lib.concatMap pkgs.mcwitt.gitignore.ghGitIgnoreLines [
      "Global/Vim"
      "Global/Emacs"
    ]) ++ [ ".direnv/" ];

    delta.enable = true;

    extraConfig = {
      # https://stackoverflow.com/a/9463536
      format.pretty = "format:%C(auto,yellow)%h%C(auto,magenta)% G? %C(auto,blue)%>(12,trunc)%ad %C(auto,green)%<(7,trunc)%aN%C(auto,reset)%s%C(auto,red)% gD% D";

      gitHub.user = "mcwitt";
      log.date = "relative";
      merge.conflictStyle = "diff3";
      pull.rebase = true;
    };
  };

  programs.home-manager.enable = true;

  programs.jq.enable = true;

  programs.kitty = {
    enable = true;
    font = { name = "Iosevka"; size = 10; };
    theme = "Solarized Dark";
  };

  programs.lsd = {
    enable = true;
    enableAliases = true;
  };

  programs.mpv.enable = true;

  programs.nix-index = {
    enable = true;
    enableFishIntegration = true;
    enableZshIntegration = true;
  };

  programs.pandoc.enable = true;

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
    extraConfig = builtins.readFile "${sources.tmux-colors-solarized}/tmuxcolors-dark.conf";
  };

  programs.vscode = {

    enable = true;

    extensions = with pkgs.vscode-extensions; [
      arrterian.nix-env-selector
      ms-toolsai.jupyter
      vscodevim.vim
    ];

    keybindings = [
      {
        key = "alt+n";
        command = "editor.action.marker.nextInFiles";
        when = "editorFocus";
      }
      {
        key = "alt+p";
        command = "editor.action.marker.prevInFiles";
        when = "editorFocus";
      }
    ];

    userSettings = {

      update.mode = "none";
      extensions.autoUpdate = false;
      workbench.colorTheme = "Solarized Dark";

      editor = {
        fontFamily = "'Iosevka'";
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

  programs.zoxide = {
    enable = true;
    enableFishIntegration = true;
    enableZshIntegration = true;
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
      bindkey fd vi-cmd-mode
      bindkey -M vicmd 'k' history-substring-search-up
      bindkey -M vicmd 'j' history-substring-search-down
    '';
  };

  shells.aliases = {
    cdr = ''cd "$(${pkgs.git}/bin/git rev-parse --show-toplevel)"'';
    ec = "${config.programs.emacs.finalPackage}/bin/emacsclient --create-frame";
    ff = "${pkgs.fd}/bin/fd";
    git = "${pkgs.hub}/bin/hub";
    g = "${pkgs.hub}/bin/hub";
    ga = "${pkgs.git-annex}/bin/git-annex";
    gb = "${pkgs.git}/bin/git b";
    gca = "${pkgs.git}/bin/git ca";
    gd = "${pkgs.git}/bin/git d";
    gds = "${pkgs.git}/bin/git ds";
    gl = "${pkgs.git}/bin/git l";
    gw = "${pkgs.git}/bin/git w";
    rm = "${pkgs.coreutils}/bin/rm -i";
  };
}
