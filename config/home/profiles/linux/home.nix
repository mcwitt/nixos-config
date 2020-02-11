{ pkgs, ... }: {
  imports = [ ../../home.nix ];

  home.packages = [ pkgs.signal-desktop ];

  programs = {
    chromium.enable = true;
    firefox.enable = true;
    git.ignores = pkgs.ghGitIgnoreLines "Global/Linux";

    zsh.shellAliases = {
      ec = "${pkgs.emacs}/bin/emacsclient";
      emacs = "${pkgs.emacs}/bin/emacsclient --create-frame";
    };
  };

  xdg.configFile.xmobar.source = "${pkgs.mcwitt-dotfiles}/config/xmobar/";

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
