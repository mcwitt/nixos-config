{
  config,
  lib,
  pkgs,
  ...
}:
{
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

    programs.feh.enable = true;

    programs.firefox = {
      enable = true;
      profiles.default = {
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
      fonts =
        let
          inherit (config.stylix) fonts;
        in
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

    xdg.desktopEntries.org-protocol = {
      name = "org-protocol";
      comment = "Intercept calls from emacsclient to trigger custom actions";
      categories = [ "X-Other" ];
      icon = "emacs";
      type = "Application";
      exec = "emacsclient -- %u";
      terminal = false;
      mimeType = [ "x-scheme-handler/org-protocol" ];
    };
  };
}
