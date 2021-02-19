{ config, pkgs, ... }:
{
  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = pkgs.writeText "xmonad.hs" ''
      import qualified Data.Map as M
      import XMonad
      import XMonad.Actions.WindowBringer (bringMenu, gotoMenu)
      import XMonad.Hooks.DynamicLog (xmobar)
      import XMonad.Layout.MultiToggle (Toggle (Toggle), mkToggle, single)
      import XMonad.Layout.NoBorders (smartBorders)
      import XMonad.Layout.Reflect (REFLECTX (REFLECTX))

      myLayoutHook =
        mkToggle (single REFLECTX)
          . smartBorders
          $ layoutHook def

      myKeys conf@(XConfig {XMonad.modMask = modm}) =
        M.fromList
          [ ((modm, xK_f), sendMessage $ Toggle REFLECTX),
            ((modm, xK_g), gotoMenu),
            ((modm, xK_b), bringMenu),
            ((modm, xK_y), spawn "${config.programs.emacs.finalPackage}/bin/emacsclient --create-frame"),
            ((modm, xK_u), spawn "chromium-browser"),
            ((modm, xK_s), spawn "${pkgs.lightdm}/bin/dm-tool switch-to-greeter")
          ]

      newKeys conf = myKeys conf `M.union` keys def conf

      main =
        xmonad
          =<< xmobar
            def
              { borderWidth = 5,
                normalBorderColor = "#073642",
                focusedBorderColor = "#859900",
                layoutHook = myLayoutHook,
                modMask = mod4Mask,
                terminal = "alacritty",
                keys = newKeys
              }
    '';
  };
}
