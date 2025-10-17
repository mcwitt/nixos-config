{
  config,
  lib,
  pkgs,
  ...
}:
{
  config = lib.mkIf config.profiles.desktop.enable {

    xsession.windowManager.xmonad = {
      enable = true;

      enableContribAndExtras = true;

      extraPackages = ps: with ps; [ dbus ];

      config =
        let
          inherit (config.stylix) fonts;
          inherit (config.lib.stylix) colors;
        in
        pkgs.writeText "xmonad.hs" ''
          import qualified Codec.Binary.UTF8.String as UTF8
          import qualified DBus as D
          import qualified DBus.Client as D
          import Data.Bifunctor (first)
          import Data.List (find)
          import Data.Maybe (catMaybes)
          import Data.Ratio ((%))
          import Graphics.X11.ExtraTypes.XF86
          import XMonad
          import XMonad.Actions.EasyMotion (selectWindow)
          import qualified XMonad.Actions.EasyMotion as EM
          import XMonad.Actions.FocusNth (swapNth)
          import XMonad.Actions.Minimize (maximizeWindowAndFocus, minimizeWindow, withLastMinimized, withMinimized)
          import XMonad.Hooks.DynamicLog (PP (..), dynamicLogWithPP, shorten, wrap)
          import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
          import XMonad.Hooks.FloatConfigureReq (fixSteamFlicker)
          import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
          import XMonad.Hooks.OnPropertyChange (onXPropertyChange)
          import XMonad.Layout.BoringWindows (boringWindows, focusDown, focusMaster, focusUp)
          import XMonad.Layout.Grid (Grid (..))
          import XMonad.Layout.Minimize (minimize)
          import XMonad.Layout.MultiToggle (Toggle (Toggle), mkToggle, single)
          import XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR))
          import XMonad.Layout.Renamed (Rename (CutWordsLeft, Replace), renamed)
          import XMonad.Layout.Spacing (spacingWithEdge)
          import XMonad.Layout.ThreeColumns (ThreeCol (ThreeCol, ThreeColMid))
          import qualified XMonad.StackSet as W
          import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)
          import XMonad.Util.NamedScratchpad (NamedScratchpad (NS), customFloating, namedScratchpadAction, namedScratchpadManageHook)

          main = do
            logOutput <- setupLogOutput
            xmonad
              . docks
              . ewmhFullscreen
              . ewmh
              $ def
                { borderWidth = 6,
                  normalBorderColor = "${colors.withHashtag.base03}",
                  focusedBorderColor = "${colors.withHashtag.base0D}",
                  layoutHook = myLayoutHook,
                  logHook = mkPolybarLogHook logOutput,
                  manageHook = myManageHook,
                  handleEventHook = myHandleEventHook,
                  modMask = mod4Mask,
                  terminal = "wezterm"
                }
                `additionalKeysP` [ -- focus
                                    ("M-j", focusDown),
                                    ("M-k", focusUp),
                                    ("M-m", focusMaster),
                                    ("M-o", easyFocus),
                                    ("M-S-o", easySwap),
                                    ("C-M-o", spawn "rofi -show-icons -show window"),
                                    -- minimize/maximize
                                    ("M-\\", withFocused minimizeWindow),
                                    ("M-S-\\", withLastMinimized maximizeWindowAndFocus),
                                    ("M-x", sendMessage $ Toggle MIRROR),
                                    -- launchers
                                    ("M-p", spawn "rofi -show-icons -show drun"),
                                    ("M-S-p", spawn "rofi -show-icons -show run"),
                                    ("M-0", spawn "rofi-rbw --keybindings Ctrl-1:type:username:password,Ctrl-2:type:password,Ctrl-3:copy:password,Ctrl-4:type:totp"),
                                    ("M-i", spawn "rofi -show ssh"),
                                    ("M-;", spawn "rofi -show emoji"),
                                    ("M-'", spawn "rofi -show calc -no-show-match -no-sort"),
                                    ("M-y", spawn "emacsclient -c -n -e '(switch-to-buffer nil)'"),
                                    ("M-u", spawn "chromium-browser"),
                                    ("M-s", spawn "dm-tool switch-to-greeter"),
                                    ("C-M-4", spawn "flameshot gui"),
                                    ("C-M-5", spawn "flameshot launcher"),
                                    -- scratchpads
                                    ("M-S-h", namedScratchpadAction scratchpads "wezterm"),
                                    ("M-b", namedScratchpadAction scratchpads "btop"),
                                    -- ("M-n", namedScratchpadAction scratchpads "ncspot"), -- TODO: librespot broken
                                    ("M-n", namedScratchpadAction scratchpads "spotify"),
                                    ("M-S-'", namedScratchpadAction scratchpads "emacs-calc"),
                                    ("M-[", namedScratchpadAction scratchpads "emacs-org-capture"),
                                    ("M-]", namedScratchpadAction scratchpads "emacs-org-agenda")
                                  ]
                `additionalKeys` ( first (noModMask,)
                                     <$> [ (xF86XK_MonBrightnessUp, spawn "xbacklight -inc 5"),
                                           (xF86XK_MonBrightnessDown, spawn "xbacklight -dec 5"),
                                           (xF86XK_AudioMute, spawn "amixer -q set Master toggle"),
                                           (xF86XK_AudioLowerVolume, spawn "amixer -q set Master 5%-"),
                                           (xF86XK_AudioRaiseVolume, spawn "amixer -q set Master 5%+")
                                         ]
                                 )

          myLayoutHook =
            diminish (spacingWithEdge 6)
              . diminish minimize
              . boringWindows
              . avoidStruts
              . mkToggle (single MIRROR)
              $ layout
            where
              layout =
                ThreeCol 1 (3 % 100) (1 % 2)
                  ||| renamed [Replace "ThreeColMid"] (ThreeColMid 1 (3 % 100) (1 % 2))
                  ||| Grid
                  ||| Full

              diminish = (renamed [CutWordsLeft 1] .)

          mkPolybarLogHook :: (String -> IO ()) -> X ()
          mkPolybarLogHook output = do
            windowCount <- withWindowSet (pure . length . W.index)
            minimizedCount <- withMinimized (pure . length)
            dynamicLogWithPP $
              def
                { ppSep = color "${colors.withHashtag.base03}" " | ",
                  ppTitle = shorten 80,
                  ppLayout =
                    let windowCountLabel =
                          unwords $
                            catMaybes
                              [ show <$> positive windowCount,
                                color "${colors.withHashtag.base04}" . wrap "(" ")" . show <$> positive minimizedCount
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

          myManageHook =
            composeAll
              [ manageDocks,
                namedScratchpadManageHook scratchpads,
                manageZoomHook
              ]

          myHandleEventHook =
            fixSteamFlicker
              <+> onXPropertyChange "WM_NAME" manageZoomHook
              <+> handleEventHook def

          -- https://www.peterstuart.org/posts/2021-09-06-xmonad-zoom/
          manageZoomHook =
            composeAll
              [ (className =? zoomClassName) <&&> shouldFloat <$> title --> doFloat,
                (className =? zoomClassName) <&&> shouldSink <$> title --> doSink
              ]
            where
              zoomClassName = "zoom"
              tileTitles =
                [ "Zoom - Free Account", -- main window
                  "Zoom - Licensed Account", -- main window
                  "Zoom", -- meeting window on creation
                  "Zoom Meeting" -- meeting window shortly after creation
                ]
              shouldFloat title = title `notElem` tileTitles
              shouldSink title = title `elem` tileTitles
              doSink = (ask >>= doF . W.sink) <+> doF W.swapDown

          emConfig =
            def
              { EM.emFont = "xft:${fonts.monospace.name}:bold:size=18:antialias=true",
                EM.bgCol = "${colors.withHashtag.base04}",
                EM.borderCol = "${colors.withHashtag.base03}",
                EM.borderPx = 6,
                EM.cancelKey = xK_Escape,
                EM.overlayF = EM.fixedSize 100 100,
                EM.txtCol = "${colors.withHashtag.base01}"
              }

          easyFocus :: X ()
          easyFocus = do
            win <- selectWindow emConfig
            whenJust win (windows . W.focusWindow)

          -- https://www.reddit.com/r/xmonad/comments/rnpron/comment/hptqkh6
          easySwap :: X ()
          easySwap = do
            win <- selectWindow emConfig
            stack <- gets $ W.index . windowset
            let match = find ((win ==) . Just . fst) $ zip stack [0 ..]
            whenJust match $ swapNth . snd

          scratchpads =
            [ NS
                "wezterm"
                "wezterm start --class wezterm-scratchpad"
                (className =? "wezterm-scratchpad")
                scratchpadFloat,
              mkTermAppScratchpad "btop" scratchpadFloat,
              mkTermAppScratchpad "ncspot" scratchpadFloat,
              NS
                "spotify"
                "spotify"
                (className =? "Spotify")
                scratchpadFloat,
              NS
                "emacs-org-capture"
                "emacsclient -c -n -F '((name . \"org-capture-dedicated\"))' -e '(org-capture nil \"t\")'"
                (title =? "org-capture-dedicated")
                scratchpadFloat,
              NS
                "emacs-org-agenda"
                "emacsclient -c -n -F '((name . \"org-agenda-dedicated\"))' -e '(let ((org-agenda-window-setup (quote current-frame))) (org-agenda nil \"n\"))'"
                (title =? "org-agenda-dedicated")
                scratchpadFloat,
              NS
                "emacs-calc"
                "emacsclient -c -n -F '((name . \"full-calc-dedicated\"))' -e '(full-calc)'"
                (title =? "full-calc-dedicated")
                scratchpadFloat
            ]
            where
              mkTermAppScratchpad prog =
                NS
                  prog
                  (unwords ["wezterm", "start", "--class", windowClass, prog])
                  (className =? windowClass)
                where
                  windowClass = prog ++ "-scratchpad"

              scratchpadFloat = customFloating $ W.RationalRect (3 / 10) (1 / 5) (2 / 5) (3 / 5)
        '';
    };
  };
}
