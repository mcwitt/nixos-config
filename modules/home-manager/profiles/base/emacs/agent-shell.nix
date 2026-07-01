{
  config,
  lib,
  pkgs,
  ...
}:
{
  config = lib.mkIf config.profiles.base.enable {
    programs.emacs = {
      overrides = self: _: {
        agent-shell-manager =
          let
            rev = "53b73f13ed1ac9d2de128465a8504a7265490ea7";
          in
          self.trivialBuild {
            pname = "agent-shell-manager";
            version = "0-unstable-2026-02-13";
            src = pkgs.fetchFromGitHub {
              owner = "jethrokuan";
              repo = "agent-shell-manager";
              inherit rev;
              hash = "sha256-JPB/OnOhYbM0LMirSYQhpB6hW8SAg0Ri6buU8tMP7rA=";
            };
            packageRequires = [ self.agent-shell ];

            postPatch = ''
              substituteInPlace agent-shell-manager.el \
                --replace-fail "(agent-shell t)" "(agent-shell '(4))"
            '';
          };
      };

      extraPackages = epkgs: [
        epkgs.agent-shell
        epkgs.agent-shell-manager
      ];

      init.usePackage.agent-shell = {
        enable = true;
        command = [ "agent-shell" ];
        init = ''
          (with-eval-after-load 'project
            (add-to-list 'project-switch-commands
                         '(agent-shell "Agent shell" ?a) t))
        '';
        config = ''
          ;; Minimal UI: text header instead of the tall graphical banner.
          (setopt agent-shell-header-style 'text)

          (setopt agent-shell-openai-codex-acp-command
                  '("${pkgs.codex-acp}/bin/codex-acp"))
          (setopt agent-shell-openai-authentication
                  (agent-shell-openai-make-authentication :login t))

          (setopt agent-shell-opencode-acp-command
                  '("${pkgs.opencode}/bin/opencode" "acp"))

          (setopt agent-shell-pi-acp-command
                  '("${pkgs.pi-acp}/bin/pi-acp"))
        '';
      };

      init.usePackage.agent-shell-manager = {
        enable = true;
        bind = {
          "C-c C-'" = "agent-shell-manager-toggle";
        };
        config = ''
          (setopt agent-shell-manager-side 'bottom)

          ;; agent-shell-manager-mode derives from tabulated-list-mode, which
          ;; evil-collection puts in motion state -- but it has no
          ;; evil-collection support, so its single-key commands were shadowed.
          ;; Rebind them in motion state, keeping h/j/k/l and gg/G movement; k
          ;; and l collide with motions, so kill moves to K and logging to L.
          (with-eval-after-load 'evil
            (evil-set-initial-state 'agent-shell-manager-mode 'motion)
            (evil-define-key 'motion agent-shell-manager-mode-map
              (kbd "RET") #'agent-shell-manager-goto
              "gr" #'agent-shell-manager-refresh
              "q" #'quit-window
              "K" #'agent-shell-manager-kill
              "c" #'agent-shell-manager-new
              "r" #'agent-shell-manager-restart
              "d" #'agent-shell-manager-delete-killed
              "m" #'agent-shell-manager-set-mode
              "M" #'agent-shell-manager-set-model
              (kbd "C-c C-c") #'agent-shell-manager-interrupt
              "t" #'agent-shell-manager-view-traffic
              "L" #'agent-shell-manager-toggle-logging))
        '';
      };
    };
  };
}
