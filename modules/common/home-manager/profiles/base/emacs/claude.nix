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
          pname = "claude-code";
          version = "0.4.5";
          src = inputs."claude-code.el";
          propagatedBuildInputs = [ epkgs.inheritenv ];
        })
        (epkgs.melpaBuild {
          pname = "monet";
          version = "0.0.3";
          src = inputs.monet;
          propagatedBuildInputs = [ epkgs.websocket ];
        })
      ];

      init.usePackage = {
        claude-code = {
          enable = true;
          init = ''
            (setq claude-code-terminal-backend 'vterm)
          '';
          config = ''
            (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
            (monet-mode 1)

            (claude-code-mode)
          '';
          extraConfig = ''
            :bind-keymap
            ("C-c d" . claude-code-command-map)
          '';
        };

        monet.enable = true;
        vterm.enable = true;
      };
    };
  };
}
