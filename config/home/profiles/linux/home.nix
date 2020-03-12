{ pkgs, ... }: {
  imports = [ ../../home.nix ./org-notes-sync.nix ];

  home.packages = with pkgs; [ anki signal-desktop ];

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

    password-store.package =
      pkgs.pass.withExtensions (exts: with exts; [ pass-update pass-otp ]);

    zsh.shellAliases = {
      ec = "${pkgs.emacsEnv}/bin/emacsclient";
      emacs = "${pkgs.emacsEnv}/bin/emacsclient --create-frame";
    };
  };

  xdg.configFile.xmobar.source = "${pkgs.mcwitt-dotfiles}/config/xmobar/";

  services = {
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 14400; # 4 hours
      maxCacheTtl = 14400;
    };

    org-notes-sync = {
      enable = true;
      repoPath = "${builtins.getEnv "HOME"}/src/org-notes/";
      frequency = "*:0/5";
    };

    password-store-sync.enable = true;

    random-background = {
      enable = true;
      imageDirectory = "%h/.background-images";
    };
  };
}
