{ pkgs, ... }: {
  imports = [ ../../config/home.nix ];

  programs.git.ignores = pkgs.ghGitIgnore "Global/Linux";

  programs.zsh.shellAliases = {
    ec = "${pkgs.emacs}/bin/emacsclient";
    emacs = "${pkgs.emacs}/bin/emacsclient --create-frame";
  };

  xdg.configFile.xmobar.source = (pkgs.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "8e065aa896c2f931d5a18e0643b9f0d913e079b9";
    sha256 = "0fffspz1d4saafpqwwx0rp3mj2a6l879rg4c9pmvbq6jcsd4153b";
  } + "/root/config/xmobar/");

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
