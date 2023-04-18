{ config, inputs, lib, pkgs, ... }:
{
  imports = [ ./emacs ./neovim ];

  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [
    delta
    difftastic
    fd
    ffmpeg
    fzf
    git-annex
    htop
    imagemagick
    ncdu
    nix-du
    (parallel-full.override { willCite = true; })
    pre-commit
    proselint
    ripgrep
    xclip
    yq
  ];

  home.shellAliases = {
    cdr = ''cd "$(${pkgs.git}/bin/git rev-parse --show-toplevel)"'';
    ec = "${config.programs.emacs.finalPackage}/bin/emacsclient --create-frame";
    ff = "${pkgs.fd}/bin/fd";
    g = "${pkgs.git}/bin/git";
    ga = "${pkgs.git-annex}/bin/git-annex";
    gb = "${pkgs.git}/bin/git b";
    gca = "${pkgs.git}/bin/git ca";
    gd = "${pkgs.git}/bin/git d";
    gds = "${pkgs.git}/bin/git ds";
    gl = "${pkgs.git}/bin/git l";
    gw = "${pkgs.git}/bin/git w";
    rm = "${pkgs.coreutils}/bin/rm -i";
  };

  home.sessionVariables = {
    EDITOR = "${config.programs.emacs.finalPackage}/bin/emacsclient -c";
    ALTERNATE_EDITOR = "${pkgs.vim}/bin/vim";
  };

  languages.nix.enable = true;

  programs.bat.enable = true;

  programs.bash.enable = true;

  programs.browserpass.enable = true;

  programs.direnv.enable = true;

  programs.emacs.enable = true;

  programs.fish = {
    enable = true;

    functions = {
      fish_user_key_bindings = ''
        fish_vi_key_bindings
        bind -M insert -m default fd backward-char force-repaint
        bind -M insert ff 'commandline -i f'
      '';
    };

    interactiveShellInit = ''
      set fish_key_bindings fish_user_key_bindings
    '';

    plugins = [{ name = "fzf.fish"; src = inputs."fzf.fish"; }];
    shellAliases.cdr = lib.mkForce "cd (${pkgs.git}/bin/git rev-parse --show-toplevel)";
  };

  programs.fzf = {
    enable = true;
    enableFishIntegration = false; # prefer PatrickF1/fzf.fish
  };

  programs.gh.enable = true;

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

    ignores = (lib.concatMap lib.gitignores [
      "Global/Vim"
      "Global/Emacs"
    ]) ++ [ ".direnv/" ];

    difftastic.enable = true;

    extraConfig = {
      # https://github.com/davidshepherd7/frames-only-mode#integrating-with-command-line-git
      core.editor = "emacsclient -c";

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

  programs.wezterm = {
    enable = true;
    colorSchemes.custom = with config.lib.stylix.colors.withHashtag; {
      # https://github.com/chriskempson/base16-shell/blob/master/templates/default.mustache
      ansi = [ base00 red green yellow blue magenta cyan base05 ];
      brights = [ base03 red green yellow blue magenta cyan base07 ];

      background = base00;
      cursor_bg = base05;
      cursor_border = base05;
      cursor_fg = base00;
      foreground = base05;
      selection_bg = base05;
      selection_fg = base00;
    };
    extraConfig = let inherit (config.stylix) fonts; in ''
      return {
        font = wezterm.font '${fonts.monospace.name}',
        font_size = ${toString fonts.sizes.applications},
        color_scheme = 'custom',
        hide_tab_bar_if_only_one_tab = true,
        check_for_updates = false,
      };
    '';
  };

  programs.lsd = {
    enable = true;
    enableAliases = true;
    settings.icons.separator = "  ";
  };

  programs.mpv.enable = true;

  programs.nix-index.enable = true;

  programs.pandoc.enable = true;

  programs.password-store = {
    enable = true;
    package = pkgs.pass.withExtensions (exts: with exts; [ pass-update pass-otp ]);
    settings = {
      PASSWORD_STORE_DIR = "${config.home.homeDirectory}/.password-store/";
    };
  };

  programs.starship = {
    enable = true;
    settings.directory.fish_style_pwd_dir_length = 1;
  };

  programs.tmux = {
    enable = true;
    keyMode = "vi";
    extraConfig = builtins.readFile (config.lib.stylix.scheme inputs.base16-tmux);
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

      editor = {
        fontFamily = "'${config.stylix.fonts.monospace.name}'";
        fontLigatures = true;
        formatOnSave = true;
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

  programs.zoxide.enable = true;

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
}
