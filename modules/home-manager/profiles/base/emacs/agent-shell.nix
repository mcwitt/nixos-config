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

      init.usePackage.agent-shell = {
        enable = true;
        command = [ "agent-shell" ];
        init = ''
          (with-eval-after-load 'project
            (keymap-set project-prefix-map "a" #'agent-shell)
            (add-to-list 'project-switch-commands
                         '(agent-shell "Agent shell") t))
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

          ;; evil shadows the mode's plain single-key bindings; re-express them
          ;; as a normal-state aux (falls through to evil-collection's
          ;; tabulated-list bindings). k/l are motions, so kill->K, logging->L.
          (with-eval-after-load 'evil
            (evil-define-key* 'normal agent-shell-manager-mode-map
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
