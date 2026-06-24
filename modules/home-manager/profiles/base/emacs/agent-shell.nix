{
  config,
  lib,
  pkgs,
  ...
}:
{
  config = lib.mkIf config.profiles.base.enable {
    programs.emacs = {
      extraPackages = epkgs: [ epkgs.agent-shell ];

      init.usePackage.agent-shell = {
        enable = true;
        bind = {
          "C-c C-'" = "agent-shell";
        };
        config = ''
          (setopt agent-shell-openai-codex-acp-command
                  '("${pkgs.codex-acp}/bin/codex-acp"))
          (setopt agent-shell-openai-authentication
                  (agent-shell-openai-make-authentication :login t))

          (setopt agent-shell-opencode-acp-command
                  '("${pkgs.opencode}/bin/opencode" "acp"))
        '';
      };
    };
  };
}
