{ pkgs, ... }: {
  imports = [ ../../home.nix ];

  home.packages = [ pkgs.signal-desktop ];

  nixpkgs.config.packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball
      "https://github.com/nix-community/NUR/archive/master.tar.gz") {
        inherit pkgs;
      };
  };

  programs = {
    chromium.enable = true;

    firefox = {
      enable = true;

      extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        browserpass
        https-everywhere
        privacy-badger
        ublock-origin
        vimium
      ];
    };

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
