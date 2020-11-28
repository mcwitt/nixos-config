{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.programs.emacs.org-protocol;
  orgProtocolDesktopItem = pkgs.makeDesktopItem rec {
    name = "org-protocol";
    desktopName = name;
    mimeType = "x-scheme-handler/org-protocol";
    exec = "${config.programs.emacs.finalPackage}/bin/emacsclient %u";
    icon = "emacs";
    type = "Application";
    terminal = "false";
    categories = "System";
  };
in
{
  options.programs.emacs.org-protocol.enable =
    mkEnableOption "Org-protocol desktop item";

  config = mkIf cfg.enable { home.packages = [ orgProtocolDesktopItem ]; };
}
