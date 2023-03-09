{ config, lib, pkgs, ... }:
{
  xsession.windowManager.xmonad = {
    enable = true;

    enableContribAndExtras = true;

    extraPackages = ps: [ ps.dbus ];

    config = with config.scheme.withHashtag; pkgs.writeText "xmonad.hs" ''
      import Data.List (isInfixOf)
      import Data.Ratio ((%))
      import XMonad
      import XMonad.Actions.Minimize (maximizeWindowAndFocus, minimizeWindow, withLastMinimized)
      import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
      import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
      import XMonad.Layout.BoringWindows (boringWindows, focusDown, focusMaster, focusUp)
      import XMonad.Layout.Minimize (minimize)
      import XMonad.Layout.MultiToggle (Toggle (Toggle), mkToggle, single)
      import XMonad.Layout.Spacing (spacingWithEdge)
      import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)

      main =
        xmonad
          . docks
          . ewmhFullscreen
          . ewmh
          $ def
            { borderWidth = 5,
              normalBorderColor = "${base00}",
              focusedBorderColor = "${base0D}",
              layoutHook =
                let tall = Tall 1 (1 % 50) (3 % 5)
                    layouts = tall ||| Mirror tall ||| Full
                 in spacingWithEdge 8
                      . avoidStruts
                      . minimize
                      . boringWindows
                      $ layouts,
              manageHook = manageDocks,
              modMask = mod4Mask,
              terminal = "wezterm"
            }
            `additionalKeys` [ ((mod4Mask, xK_j), focusDown),
                               ((mod4Mask, xK_k), focusUp),
                               ((mod4Mask, xK_m), focusMaster),
                               ((mod4Mask, xK_backslash), withFocused minimizeWindow),
                               ((mod4Mask + shiftMask, xK_backslash), withLastMinimized maximizeWindowAndFocus),
                               ((mod4Mask, xK_g), spawn "rofi -show-icons -show window"),
                               ((mod4Mask, xK_p), spawn "rofi -show-icons -show drun"),
                               ((mod4Mask + shiftMask, xK_p), spawn "rofi -show run"),
                               ((mod4Mask, xK_o), spawn "rofi-pass"),
                               ((mod4Mask, xK_i), spawn "rofi -show ssh"),
                               ((mod4Mask, xK_semicolon), spawn "rofi -show emoji"),
                               ((mod4Mask, xK_quoteright), spawn "rofi -show calc"),
                               ((mod4Mask, xK_y), spawn "emacsclient -c -n -e '(switch-to-buffer nil)'"),
                               ((mod4Mask, xK_u), spawn "chromium-browser"),
                               ((mod4Mask, xK_s), spawn "dm-tool switch-to-greeter")
                             ]
            `additionalKeysP` [ ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 2"),
                                ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 2"),
                                ("<XF86AudioMute>", spawn "amixer -q set Master toggle"),
                                ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 2%-"),
                                ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 2%+")
                              ]
    '';
  };
}
