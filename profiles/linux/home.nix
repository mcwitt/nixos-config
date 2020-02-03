{ pkgs, ... }: {
  imports = [ ../../config/home.nix ];

  programs.zsh.shellAliases = {
    ec = "${pkgs.emacs}/bin/emacsclient";
    emacs = "${pkgs.emacs}/bin/emacsclient --create-frame";
  };

  services = {
    emacs.enable = true;
    gpg-agent.enable = true;
  };
}
