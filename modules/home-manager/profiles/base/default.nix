{
  config,
  lib,
  pkgs,
  ...
}:
let
  # Pick a GUI frame (-c) when a display is present and we're local; otherwise
  # a terminal frame (-t), for emacsclient over SSH / in a tty.
  emacsclient-auto = pkgs.writeShellScriptBin "emacsclient-auto" ''
    if [ -n "$SSH_CONNECTION" ] || { [ -z "$DISPLAY" ] && [ -z "$WAYLAND_DISPLAY" ]; }; then
      exec ${config.programs.emacs.finalPackage}/bin/emacsclient -t "$@"
    else
      exec ${config.programs.emacs.finalPackage}/bin/emacsclient -c "$@"
    fi
  '';
in
{
  options.profiles.base.enable = lib.mkEnableOption "Base configuration enabled on most machines";

  imports = [
    ./harnesses
    ./emacs
    ./git-annex.nix
    ./jupyter.nix
    ./neovim
    ./worktrunk.nix
  ];

  config = lib.mkIf config.profiles.base.enable (
    lib.mkMerge [
      {
        home.packages = [
          emacsclient-auto
        ]
        ++ (with pkgs; [
          delta
          docker-compose
          ffmpeg
          imagemagick
          llm-agents.qmd
          nix-du
          nix-output-monitor
          prek
          prettier
          yq
        ]);

        home.sessionPath = [
          "$HOME/.local/bin"
        ];

        home.shellAliases = {
          cdr = ''cd "$(${pkgs.git}/bin/git rev-parse --show-toplevel)"'';
          ec = lib.getExe emacsclient-auto;
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
          EDITOR = lib.getExe emacsclient-auto;
          ALTERNATE_EDITOR = "${pkgs.vim}/bin/vim";
        };

        languages.nix.enable = true;

        # NOTE: if hm activation fails, delete ~/cache
        # https://github.com/nix-community/home-manager/issues/4826
        programs.bat.enable = true;

        programs.bash.enable = true;

        programs.btop = {
          enable = true;
          settings = {
            vim_keys = true;
          };
        };

        programs.difftastic = {
          enable = true;
          git.enable = true;
        };

        programs.direnv = {
          enable = true;
          nix-direnv.enable = true;
        };

        programs.fd.enable = true;

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

        programs.fzf.enable = true;

        programs.gh.enable = true;

        programs.git = {
          enable = true;

          settings = {
            alias = {
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

            # https://github.com/davidshepherd7/frames-only-mode#integrating-with-command-line-git
            # emacsclient-auto picks -t (TUI) over SSH / in a tty, -c (GUI) locally.
            core.editor = lib.getExe emacsclient-auto;

            # https://stackoverflow.com/a/9463536
            format.pretty = "format:%C(auto,yellow)%h%C(auto,magenta)% G? %C(auto,blue)%>(12,trunc)%ad %C(auto,green)%<(7,trunc)%aN%C(auto,reset)%s%C(auto,red)% gD% D";

            log.date = "relative";
            merge.conflictStyle = "diff3";
            pull.rebase = true;
          };

          ignores =
            (lib.concatMap pkgs.gitignores [
              "Global/Vim"
              "Global/Emacs"
              "Global/Linux"
            ])
            ++ [ ".direnv/" ];
        };

        programs.gpg.enable = true;

        programs.home-manager.enable = true;

        programs.jq.enable = true;

        programs.ncdu.enable = true;

        programs.nushell.enable = true;

        programs.nix-index.enable = true;

        programs.pandoc.enable = true;

        programs.ripgrep.enable = true;

        programs.starship = {
          enable = true;
          settings.directory.fish_style_pwd_dir_length = 1;
        };

        programs.zsh = {
          enable = true;
          dotDir = "${config.xdg.configHome}/zsh";
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

      }
      (lib.mkIf pkgs.stdenv.isLinux {
        fonts.fontconfig.enable = true;

        home.shellAliases.open = "${pkgs.xdg-utils}/bin/xdg-open";

        # zellij is the persistent multiplexer (replaces tmux). Linux-only: the
        # MacBook reaches zellij over SSH on the desktop, never locally.
        programs.zellij = {
          enable = true;
          settings = {
            # "unlock-first" / non-colliding preset: start locked so every
            # Ctrl- key passes through to emacs/shell; Ctrl-g unlocks.
            default_mode = "locked";
            # Persist the session across reboots, including pane scrollback.
            session_serialization = true;
            serialize_pane_viewport = true;
          };
        };

        services.emacs = {
          enable = true;
          client.enable = true;
        };

        # Plain ssh-agent holds keys in memory only; load them manually once per
        # boot with e.g. `ssh-add ~/.ssh/id_ed25519` (they persist until
        # shutdown). NOTE: keychain was tried for automating this and removed —
        # keychain 2.9 spawns its own agent under ~/.ssh/agent/ instead of
        # inheriting this one, so keys ended up invisible to systemd user
        # services like the emacs daemon.
        services.ssh-agent.enable = true;

        # The ssh-agent module exports SSH_AUTH_SOCK via shell init only; publish
        # it to the systemd user environment (environment.d) as well so user
        # services — notably the emacs daemon, which signs commits via magit —
        # can reach the agent.
        systemd.user.sessionVariables.SSH_AUTH_SOCK = "$XDG_RUNTIME_DIR/ssh-agent";
      })
    ]
  );
}
