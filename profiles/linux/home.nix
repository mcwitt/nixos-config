{ pkgs, ... }: {
  imports = [ ../../config/home.nix ];

  programs.zsh.shellAliases = {
    ec = "${pkgs.emacs}/bin/emacsclient";
    emacs = "${pkgs.emacs}/bin/emacsclient --create-frame";
  };

  services = {
    emacs.enable = true;

    gpg-agent = {
      enable = true;
      defaultCacheTtl = 14400; # 4 hours
      maxCacheTtl = 14400;
    };

    password-store-sync.enable = true;

    random-background = {
      enable = true;
      imageDirectory = "%h/.background-images";
    };
  };
}
