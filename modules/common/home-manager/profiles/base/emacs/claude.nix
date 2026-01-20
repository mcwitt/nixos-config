{
  config,
  inputs,
  lib,
  ...
}:
{
  config = lib.mkIf config.profiles.base.enable {
    programs.emacs = {
      extraPackages = epkgs: [
        (epkgs.melpaBuild {
          pname = "claude-code-ide";
          version = "20260102.1802";
          src = inputs."claude-code-ide.el";
          propagatedBuildInputs = with epkgs; [
            transient
            websocket
            web-server
          ];
        })
      ];

      init.usePackage = {
        claude-code-ide = {
          enable = true;
          bind = {
            "C-c C-'" = "claude-code-ide-menu";
          };
          config = ''
            (claude-code-ide-emacs-tools-setup)
          '';
        };

        vterm.enable = true;
      };
    };
  };
}
