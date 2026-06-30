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
          };
      };

      extraPackages = epkgs: [
        epkgs.agent-shell
        epkgs.agent-shell-manager
      ];

      init.usePackage.agent-shell = {
        enable = true;
        command = [ "agent-shell" ];
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
        '';
      };
    };
  };
}
