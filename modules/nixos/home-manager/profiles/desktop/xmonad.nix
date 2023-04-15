{ config, lib, pkgs, ... }:
{
  config = lib.mkIf config.profiles.desktop.enable {

    xsession.windowManager.xmonad = {
      enable = true;

      enableContribAndExtras = true;

      extraPackages = ps: [ ps.dbus ];

      config = with config.lib.stylix.colors.withHashtag; pkgs.writeText "xmonad.hs" ''
        {-# LANGUAGE TupleSections #-}

        import qualified Codec.Binary.UTF8.String as UTF8
        import qualified DBus as D
        import qualified DBus.Client as D
        import Data.Bifunctor (first)
        import Data.Maybe (catMaybes)
        import Data.Ratio ((%))
        import Graphics.X11.ExtraTypes.XF86
        import XMonad
        import XMonad.Actions.Minimize (maximizeWindowAndFocus, minimizeWindow, withLastMinimized, withMinimized)
        import XMonad.Hooks.DynamicLog (PP (..), dynamicLogWithPP, shorten, wrap)
        import XMonad.Hooks.EwmhDesktops (ewmh)
        import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
        import XMonad.Layout.BoringWindows (boringWindows, focusDown, focusMaster, focusUp)
        import XMonad.Layout.Minimize (minimize)
        import XMonad.Layout.Renamed (Rename (CutWordsLeft), renamed)
        import XMonad.Layout.Spacing (spacingWithEdge)
        import qualified XMonad.StackSet as W
        import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)

        main = do
          logOutput <- setupLogOutput
          xmonad
            . docks
            . ewmh
            $ def
              { borderWidth = 5,
                normalBorderColor = "${base00}",
                focusedBorderColor = "${base0D}",
                layoutHook = myLayoutHook,
                logHook = mkPolybarLogHook logOutput,
                manageHook = manageDocks,
                modMask = mod4Mask,
                terminal = "wezterm"
              }
              `additionalKeysP` [ ("M-j", focusDown),
                                  ("M-k", focusUp),
                                  ("M-m", focusMaster),
                                  ("M-\\", withFocused minimizeWindow),
                                  ("M-S-\\", withLastMinimized maximizeWindowAndFocus),
                                  ("M-g", spawn "rofi -show-icons -show window"),
                                  ("M-p", spawn "rofi -show-icons -show drun"),
                                  ("M-S-p", spawn "rofi -show-icons -show run"),
                                  ("M-o", spawn "rofi-pass"),
                                  ("M-i", spawn "rofi -show ssh"),
                                  ("M-;", spawn "rofi -show emoji"),
                                  ("M-'", spawn "rofi -show calc -no-show-match -no-sort"),
                                  ("M-y", spawn "emacsclient -c -n -e '(switch-to-buffer nil)'"),
                                  ("M-u", spawn "chromium-browser"),
                                  ("M-s", spawn "dm-tool switch-to-greeter")
                                ]
                `additionalKeys` ( first (noModMask,)
                                     <$> [ (xF86XK_MonBrightnessUp, spawn "xbacklight -inc 2"),
                                           (xF86XK_MonBrightnessDown, spawn "xbacklight -dec 2"),
                                           (xF86XK_AudioMute, spawn "amixer -q set Master toggle"),
                                           (xF86XK_AudioLowerVolume, spawn "amixer -q set Master 2%-"),
                                           (xF86XK_AudioRaiseVolume, spawn "amixer -q set Master 2%+")
                                         ]
                                 )

        myLayoutHook = do
          diminish (spacingWithEdge 8)
            . diminish minimize
            . boringWindows
            . avoidStruts
            $ layout
          where
            layout = tall ||| Full
            tall = Tall 1 (1 % 50) (3 % 5)
            diminish = (renamed [CutWordsLeft 1] .)

        mkPolybarLogHook :: (String -> IO ()) -> X ()
        mkPolybarLogHook output = do
          windowCount <- withWindowSet (pure . length . W.index)
          minimizedCount <- withMinimized (pure . length)
          dynamicLogWithPP $
            def
              { ppSep = color "${base03}" " | ",
                ppTitle = shorten 80,
                ppLayout =
                  let windowCountLabel =
                        unwords $
                          catMaybes
                            [ show <$> positive windowCount,
                              color "${base0A}" . wrap "(" ")" . show <$> positive minimizedCount
                            ]
                   in (++ " " ++ windowCountLabel),
                ppOrder = \(_ : l : win : _) -> [l, win],
                ppOutput = output
              }
          where
            positive n = if n > 0 then Just n else Nothing
            color c = wrap ("%{F" ++ c ++ "}") "%{F-}"

        setupLogOutput :: IO (String -> IO ())
        setupLogOutput = do
          dbus <- D.connectSession

          D.requestName
            dbus
            (D.busName_ "org.xmonad.Log")
            [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

          let logOutput str = do
                let signal = (D.signal objectPath interfaceName memberName) {D.signalBody = [D.toVariant (UTF8.decodeString str)]}
                D.emit dbus signal
                where
                  objectPath = D.objectPath_ "/org/xmonad/Log"
                  interfaceName = D.interfaceName_ "org.xmonad.Log"
                  memberName = D.memberName_ "Update"

          pure logOutput
      '';
    };
  };
}
