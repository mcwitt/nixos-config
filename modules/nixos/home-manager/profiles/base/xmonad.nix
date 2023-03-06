{ config, lib, pkgs, ... }:
{
  xsession.windowManager.xmonad = {
    enable = true;

    enableContribAndExtras = true;

    extraPackages = ps: [ ps.dbus ];

    config = with config.scheme.withHashtag; pkgs.writeText "xmonad.hs" ''
      {-# LANGUAGE ImportQualifiedPost #-}

      import Codec.Binary.UTF8.String qualified as UTF8
      import DBus qualified as D
      import DBus.Client qualified as D
      import Data.List (isInfixOf)
      import Data.Ratio ((%))
      import XMonad
      import XMonad.Actions.Minimize
      import XMonad.Hooks.DynamicLog
      import XMonad.Hooks.EwmhDesktops
      import XMonad.Hooks.ManageDocks (ToggleStruts (ToggleStruts), avoidStruts, docks, manageDocks)
      import XMonad.Layout.BoringWindows (boringWindows, focusDown, focusMaster, focusUp)
      import XMonad.Layout.Minimize
      import XMonad.Layout.MultiToggle (Toggle (Toggle), mkToggle, single)
      import XMonad.Layout.Reflect (REFLECTX (REFLECTX))
      import XMonad.Layout.Spacing
      import XMonad.Prompt
      import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)
      import XMonad.Util.Run (hPutStrLn, spawnPipe)

      myLayoutHook =
        let tall = Tall 1 (1 % 50) (3 % 5)
            layouts = tall ||| Mirror tall ||| Full
            minimizeBoring = minimize . boringWindows
         in spacingWithEdge 8
              . avoidStruts
              . mkToggle (single REFLECTX)
              . minimizeBoring
              $ layouts

      myPromptConfig =
        def
          { position = Top,
            promptBorderWidth = 0,
            defaultText = "",
            alwaysHighlight = True,
            height = 36,
            font = "xft:Iosevka Comfy:size=10:bold:antialias=true",
            searchPredicate = isInfixOf
          }

      myLogHook :: D.Client -> PP
      myLogHook dbus = def {ppOutput = dbusOutput dbus}

      dbusOutput :: D.Client -> String -> IO ()
      dbusOutput dbus str = do
        let signal = (D.signal objectPath interfaceName memberName) {D.signalBody = [D.toVariant $ UTF8.decodeString str]}
        D.emit dbus signal
        where
          objectPath = D.objectPath_ "/org/xmonad/Log"
          interfaceName = D.interfaceName_ "org.xmonad.Log"
          memberName = D.memberName_ "Update"

      main = do
        dbus <- D.connectSession
        D.requestName
          dbus
          (D.busName_ "org.xmonad.Log")
          [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
        xmonad
          . docks
          . ewmh
          $ def
            { borderWidth = 5,
              normalBorderColor = "${base00}",
              focusedBorderColor = "${base0D}",
              layoutHook = myLayoutHook,
              logHook = dynamicLogWithPP (myLogHook dbus),
              manageHook = manageDocks,
              modMask = mod4Mask,
              terminal = "wezterm"
            }
            `additionalKeys` [ ((mod4Mask, xK_j), focusDown),
                               ((mod4Mask, xK_k), focusUp),
                               ((mod4Mask, xK_m), focusMaster),
                               ((mod4Mask, xK_backslash), withFocused minimizeWindow),
                               ((mod4Mask + shiftMask, xK_backslash), withLastMinimized maximizeWindowAndFocus),
                               ((mod4Mask, xK_f), sendMessage $ Toggle REFLECTX),
                               ((mod4Mask, xK_b), sendMessage ToggleStruts),
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
