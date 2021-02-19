{ config, lib, pkgs, ... }:
{
  imports = [ ./alacritty.nix ./xmonad.nix ];

  home.packages = with pkgs; [
    dmenu
    libnotify
    peek
    signal-desktop
  ];

  programs.chromium = {
    enable = true;
    extensions =
      let
        browserpass = "naepdomgkenhinolocfifgehidddafch";
        dark-reader = "eimadpbcbfnmbkopoojfekhnkhdbieeh";
        https-everywhere = "gcbommkclmclpchllfjekcdonpmejbdp";
        link-to-text-fragment = "pbcodcjpfjdpcineamnnmbkkmkdpajjg";
        privacy-badger = "pkehgijcmpdhfbdbbnkijodmdjhbjlgp";
        ublock-origin = "cjpalhdlnbpafiamejdnhcphjbkeiagm";
        vimium = "dbepggeogbaibhgnhhndojpepiihcmeb";
      in
      [
        browserpass
        dark-reader
        https-everywhere
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
      https-everywhere
      privacy-badger
      ublock-origin
      vimium
    ];
    profiles.default.settings = {
      "network.protocol-handler.expose.org-protocol" = true;
    };
  };

  programs.git.ignores = pkgs.mypkgs.gitignore.ghGitIgnoreLines "Global/Linux";

  programs.xmobar = {
    enable = true;
    commands =
      [
        ''
          Run Weather "KSFO" [ "--template", "<skyCondition> | <fc=#268bd2><tempF></fc>°F | <fc=#268bd2><rh></fc>% | <fc=#268bd2><pressure></fc>hPa"
                             ] 36000''

        ''
          Run DynNetwork     [ "--template" , "<dev>: <tx>kB/s|<rx>kB/s"
                             , "--Low"      , "1000"
                             , "--High"     , "5000"
                             , "--low"      , "#859900"
                             , "--normal"   , "#b58900"
                             , "--high"     , "#dc322f"
                             ] 10
        ''
        ''
          Run MultiCpu       [ "--template" , "Cpu: <total0>% <total1>% <total2>% <total3>% <total4>% <total5>% <total6>% <total7>%"
                             , "--Low"      , "50"
                             , "--High"     , "85"
                             , "--low"      , "#859900"
                             , "--normal"   , "#b58900"
                             , "--high"     , "#dc322f"
                             , "--ppad"     , "3"
                             ] 10
        ''
        ''
          Run CoreTemp       [ "--template" , "Temp: <core0>°C <core1>°C <core2>°C <core3>°C"
                             , "--Low"      , "70"
                             , "--High"     , "80"
                             , "--low"      , "#859900"
                             , "--normal"   , "#b58900"
                             , "--high"     , "#dc322f"
                             ] 50
        ''
        ''
          Run Memory         [ "--template" ,"Mem: <usedratio>%"
                             , "--Low"      , "20"
                             , "--High"     , "90"
                             , "--low"      , "#859900"
                             , "--normal"   , "#b58900"
                             , "--high"     , "#dc322f"
                             ] 10
        ''
        ''Run Date "<fc=#93a1a1>%F (%a) %T</fc>" "date" 10''
        ''Run StdinReader''
      ];
    config = let inherit (lib) mkDefault; in
      {
        position = mkDefault "Top";
        font = mkDefault ''"xft:Fira Code:size=11:bold:antialias=true"'';
        template = mkDefault ''"%StdinReader% | %multicpu% | %coretemp% | %memory% | %dynnetwork% }{ %KSFO% | %date% "'';
        bgColor = ''"#002b36"'';
        fgColor = ''"#839496"'';
        sepChar = ''"%"'';
        alignSep = ''"}{"'';
        lowerOnStart = true;
        hideOnStart = false;
        allDesktops = true;
        overrideRedirect = true;
        pickBroadest = false;
        persistent = true;
      };
  };

  programs.zathura.enable = true;

  services.dunst.enable = true;

  services.emacs = {
    enable = true;
    client.enable = true;
  };

  services.flameshot.enable = true;

  services.gammastep.enable = true;

  services.gnome-keyring.enable = true;

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 14400; # 4 hours
    maxCacheTtl = 14400;
  };

  services.lorri.enable = true;

  services.org-notes-sync = {
    enable = true;
    repoPath = "${config.home.homeDirectory}/src/org-notes/";
    frequency = "*:0/5";
  };

  services.password-store-sync.enable = true;

  services.random-background = {
    enable = true;
    imageDirectory = "%h/.background-images";
  };

  services.spotifyd.enable = true;

  services.stalonetray = {
    enable = true;
    config = {
      background = "#002b36";
      kludges = "force_icons_size";
      transparent = false;
    };
  };

  shells.aliases.open = "${pkgs.xdg_utils}/bin/xdg-open";

  xdg = {
    enable = true;
    mimeApps = {
      enable = true;
      defaultApplications = {
        "application/pdf" = [ "org.pwmt.zathura.desktop" ];
        "image/bmp" = [ "feh.desktop" ];
        "image/gif" = [ "feh.desktop" ];
        "image/jpeg" = [ "feh.desktop" ];
        "image/jpg" = [ "feh.desktop" ];
        "image/pjpeg" = [ "feh.desktop" ];
        "image/png" = [ "feh.desktop" ];
        "image/tiff" = [ "feh.desktop" ];
        "image/webp" = [ "feh.desktop" ];
        "image/x-bmp" = [ "feh.desktop" ];
        "image/x-pcx" = [ "feh.desktop" ];
        "image/x-png" = [ "feh.desktop" ];
        "image/x-portable-anymap" = [ "feh.desktop" ];
        "image/x-portable-bitmap" = [ "feh.desktop" ];
        "image/x-portable-graymap" = [ "feh.desktop" ];
        "image/x-portable-pixmap" = [ "feh.desktop" ];
        "image/x-tga" = [ "feh.desktop" ];
        "image/x-xbitmap" = [ "feh.desktop" ];
        "text/plain" = [ "emacsclient.desktop" ];
        "text/html" = [ "chromium-browser.desktop" ];
        "x-scheme-handler/http" = [ "chromium-browser.desktop" ];
        "x-scheme-handler/https" = [ "chromium-browser.desktop" ];
        "x-scheme-handler/ftp" = [ "chromium-browser.desktop" ];
        "x-scheme-handler/about" = [ "chromium-browser.desktop" ];
        "x-scheme-handler/unknown" = [ "chromium-browser.desktop" ];
      };
    };
  };

  xresources.extraConfig =
    builtins.readFile (pkgs.mypkgs.sources.solarized + "/xresources/solarized");

  xsession = {
    enable = true;

    pointerCursor = {
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ";
      size = 48;
    };
  };
}
