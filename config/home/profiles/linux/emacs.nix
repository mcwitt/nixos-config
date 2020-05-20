{ pkgs, config, lib, ... }: {

  home.sessionVariables.EDITOR =
    "${config.programs.emacs.package}/bin/emacsclient --tty";

  programs = {
    emacs = {
      enable = true;
      extraPackages = pkgs.mypkgs.emacsPackages;
    };

    zsh.shellAliases = {
      ec = lib.mkForce "${config.programs.emacs.package}/bin/emacsclient --tty";
      emacs = lib.mkForce
        "${config.programs.emacs.package}/bin/emacsclient --create-frame";
    };
  };

  services.emacs.enable = true;
}
