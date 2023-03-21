{ config, lib, pkgs, ... }:
{
  home.packages = with pkgs; [
    signal-desktop
    spotify
  ];

  home.shellAliases.open = "${pkgs.xdg-utils}/bin/xdg-open";

  programs.chromium = {
    enable = true;
    package = pkgs.chromium.override {
      # enable Chromecast
      commandLineArgs = "--load-media-router-component-extension=1";
    };
    extensions =
      let
        browserpass = "naepdomgkenhinolocfifgehidddafch";
        dark-reader = "eimadpbcbfnmbkopoojfekhnkhdbieeh";
        link-to-text-fragment = "pbcodcjpfjdpcineamnnmbkkmkdpajjg";
        privacy-badger = "pkehgijcmpdhfbdbbnkijodmdjhbjlgp";
        ublock-origin = "cjpalhdlnbpafiamejdnhcphjbkeiagm";
        vimium = "dbepggeogbaibhgnhhndojpepiihcmeb";
      in
      [
        browserpass
        dark-reader
        link-to-text-fragment
        privacy-badger
        ublock-origin
        vimium
      ];
  };

  programs.emacs.org-protocol.enable = true;

  programs.feh.enable = true;

  programs.firefox = {
    enable = true;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      browserpass
      darkreader
      privacy-badger
      ublock-origin
      vimium
    ];
    profiles.default.settings = {
      "network.protocol-handler.expose.org-protocol" = true;
    };
  };

  programs.git.ignores = lib.gitignores "Global/Linux";

  programs.urxvt = {
    enable = true;
    fonts = [ "xft:${config.stylix.fonts.monospace.name}:size=10:antialias=true" ];
  };

  programs.vscode.package = pkgs.vscodium-fhs;

  programs.zathura.enable = true;

  services.emacs = {
    enable = true;
    client.enable = true;
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 4 * 60 * 60;
    maxCacheTtl = 4 * 60 * 60;
  };

  services.org-notes-sync = {
    enable = true;
    repoPath = "${config.home.homeDirectory}/src/org-notes/";
    frequency = "*:0/5";
  };

  services.pass-secret-service.enable = true;

  services.password-store-sync.enable = true;
}
