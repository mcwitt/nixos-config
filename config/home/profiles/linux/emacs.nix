{ pkgs, config, ... }: {

  home.sessionVariables.EDITOR =
    "${config.programs.emacs.package}/bin/emacsclient --tty";

  programs = {
    emacs = {
      enable = true;
      package = pkgs.mypkgs.emacs;
    };

    zsh.shellAliases = {
      ec = "${config.programs.emacs.package}/bin/emacsclient --tty";
      emacs = "${config.programs.emacs.package}/bin/emacsclient --create-frame";
    };
  };

  services.emacs.enable = true;
}
