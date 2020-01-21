{ pkgs, lib, ... }:

{
  imports = [ ./fonts.nix ./packages.nix ];
  environment.systemPackages = with pkgs; [
    dmenu
    haskellPackages.xmobar
  ];

  services = {
    xserver.windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ''
        import XMonad
        import XMonad.Hooks.DynamicLog

        main = xmonad =<< xmobar def
            { terminal = "${pkgs.termite}"
            , modMask  = mod4Mask
            }
      '';
    };

    redshift.enable = true;  # color temperature adjuster
  };
}
