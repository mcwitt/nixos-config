{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.profiles.base.enable = lib.mkEnableOption "Base configuration enabled on most machines";

  imports = [
    ./emacs
    ./git-annex.nix
    ./jupyter.nix
    ./neovim
  ];

  config = lib.mkIf config.profiles.base.enable {
    fonts.fontconfig.enable = true;

    home.packages = with pkgs; [
      delta
      difftastic
      fd
      ffmpeg
      fzf
      htop
      imagemagick
      nix-du
      nix-output-monitor
      (parallel-full.override { willCite = true; })
      nodePackages.prettier
      ripgrep
      yq
    ];

    home.shellAliases = {
      cdr = ''cd "$(${pkgs.git}/bin/git rev-parse --show-toplevel)"'';
      ec = "${config.programs.emacs.finalPackage}/bin/emacsclient --create-frame";
      ff = "${pkgs.fd}/bin/fd";
      g = "${pkgs.git}/bin/git";
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

    # NOTE: if hm activation fails, delete ~/cache
    # https://github.com/nix-community/home-manager/issues/4826
    programs.bat.enable = true;

    programs.bash.enable = true;

    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

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
        set fish_greeting "🐟"
      '';

      plugins = [
        {
          name = "fzf-fish";
          inherit (pkgs.fishPlugins.fzf-fish) src;
        }
      ];

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

      ignores =
        (lib.concatMap pkgs.gitignores [
          "Global/Vim"
          "Global/Emacs"
        ])
        ++ [ ".direnv/" ];

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

    programs.gpg.enable = true;

    programs.home-manager.enable = true;

    programs.jq.enable = true;

    programs.ncdu.enable = true;

    programs.nushell.enable = true;

    programs.nix-index.enable = true;

    programs.pandoc.enable = true;

    programs.starship = {
      enable = true;
      settings.directory.fish_style_pwd_dir_length = 1;
    };

    programs.tmux = {
      enable = true;
      keyMode = "vi";
    };

    programs.zsh = {
      enable = true;
      autocd = true;
      autosuggestion.enable = true;
      defaultKeymap = "viins";
      enableCompletion = false;

      history = {
        size = 50000;
        save = 500000;
        ignoreDups = true;
        extended = true;
      };

      initContent = ''
        setopt HIST_IGNORE_SPACE
        source ${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
        source ${pkgs.zsh-history-substring-search}/share/zsh-history-substring-search/zsh-history-substring-search.zsh
        bindkey fd vi-cmd-mode
        bindkey -M vicmd 'k' history-substring-search-up
        bindkey -M vicmd 'j' history-substring-search-down
      '';
    };
  };
}
