{ config, lib, pkgs, ... }:
{
  options.profiles.base.enable = lib.mkEnableOption "Base configuration enabled on most machines";

  config = lib.mkIf config.profiles.base.enable {
    home.packages = with pkgs; [ signal-desktop ];

    home.shellAliases.open = "${pkgs.xdg-utils}/bin/xdg-open";

    programs.chromium = {
      enable = true;
      extensions =
        let
          bitwarden = "nngceckbapebfimnlniiiahkandclblb";
          dark-reader = "eimadpbcbfnmbkopoojfekhnkhdbieeh";
          link-to-text-fragment = "pbcodcjpfjdpcineamnnmbkkmkdpajjg";
          privacy-badger = "pkehgijcmpdhfbdbbnkijodmdjhbjlgp";
          ublock-origin = "cjpalhdlnbpafiamejdnhcphjbkeiagm";
          vimium = "dbepggeogbaibhgnhhndojpepiihcmeb";
        in
        [
          bitwarden
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
      profiles.default = {
        settings."network.protocol-handler.expose.org-protocol" = true;
        extensions = with pkgs.nur.repos.rycee.firefox-addons; [
          browserpass
          darkreader
          privacy-badger
          ublock-origin
          vimium
        ];
      };
    };

    programs.git.ignores = pkgs.gitignores "Global/Linux";

    programs.urxvt = {
      enable = true;
      fonts = let inherit (config.stylix) fonts; in
        [ "xft:${fonts.monospace.name}:size=${toString fonts.sizes.applications}:antialias=true" ];
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
  };
}
