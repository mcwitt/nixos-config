{ config, lib, pkgs, ... }:
{
  programs.xmobar = let cfg = config.programs.xmobar; in
    {
      enable = true;
      commands =
        [
          ''
            Run DynNetwork [ "--template" , "<dev>: <tx>kB/s|<rx>kB/s"
                           , "--Low"      , "1000"
                           , "--High"     , "5000"
                           , "--low"      , "${cfg.colors.good}"
                           , "--normal"   , "${cfg.colors.normal}"
                           , "--high"     , "${cfg.colors.alert}"
                           ] 10
          ''
          ''
            Run MultiCpu   [ "--template" , "Cpu: <total0>% <total1>% <total2>% <total3>% <total4>% <total5>% <total6>% <total7>%"
                           , "--Low"      , "50"
                           , "--High"     , "85"
                           , "--low"      , "${cfg.colors.good}"
                           , "--normal"   , "${cfg.colors.normal}"
                           , "--high"     , "${cfg.colors.alert}"
                           , "--ppad"     , "3"
                           ] 10
          ''
          ''
            Run CoreTemp   [ "--template" , "Temp: <core0>°C <core1>°C <core2>°C <core3>°C"
                           , "--Low"      , "70"
                           , "--High"     , "80"
                           , "--low"      , "${cfg.colors.good}"
                           , "--normal"   , "${cfg.colors.normal}"
                           , "--high"     , "${cfg.colors.alert}"
                           ] 50
          ''
          ''
            Run Memory     [ "--template" ,"Mem: <usedratio>%"
                           , "--Low"      , "20"
                           , "--High"     , "90"
                           , "--low"      , "${cfg.colors.good}"
                           , "--normal"   , "${cfg.colors.normal}"
                           , "--high"     , "${cfg.colors.alert}"
                           ] 10
          ''
          ''Run Date "<fc=#93a1a1>%F (%a) %T</fc>" "date" 10''
          ''Run StdinReader''
        ];
      config = let inherit (lib) mkDefault; in
        {
          position = mkDefault "Top";
          font = mkDefault ''"xft:Iosevka:size=12:bold:antialias=true"'';
          template = mkDefault ''"<fc=#d33682>%StdinReader%</fc> | %multicpu% | %coretemp% | %memory% | %dynnetwork% }{ %date% "'';
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

  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = lib.mkDefault (pkgs.writeText "xmonad.hs" ''
      import Data.List (isInfixOf)
      import XMonad
      import XMonad.Actions.WindowBringer (bringMenu, gotoMenu)
      import XMonad.Config.Desktop (desktopConfig)
      import XMonad.Hooks.DynamicLog
      import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks)
      import XMonad.Layout.MultiToggle (Toggle (Toggle), mkToggle, single)
      import XMonad.Layout.NoBorders (smartBorders)
      import XMonad.Layout.Reflect (REFLECTX (REFLECTX))
      import XMonad.Prompt
      import XMonad.Prompt.Shell (shellPrompt)
      import XMonad.Util.EZConfig (additionalKeysP)
      import XMonad.Util.Run (hPutStrLn, spawnPipe)

      myKeys =
        [ ("M-p", shellPrompt myPromptConfig),
          ("M-f", sendMessage $ Toggle REFLECTX),
          ("M-g", gotoMenu),
          ("M-b", bringMenu),
          ("M-y", spawn "${config.programs.emacs.finalPackage}/bin/emacsclient -c -n -e '(switch-to-buffer nil)'"),
          ("M-u", spawn "${config.programs.chromium.package}/bin/chromium-browser"),
          ("M-s", spawn "${pkgs.lightdm}/bin/dm-tool switch-to-greeter"),
          ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 2"),
          ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 2"),
          ("<XF86AudioMute>", spawn "amixer -q set Master toggle"),
          ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 2%-"),
          ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 2%+")
        ]

      myLayoutHook =
        avoidStruts
          . smartBorders
          . mkToggle (single REFLECTX)
          $ layoutHook def

      myPromptConfig =
        def
          { position = Top,
            promptBorderWidth = 0,
            defaultText = "",
            alwaysHighlight = True,
            height = 36,
            font = "xft:Iosevka:size=12:bold:antialias=true",
            searchPredicate = isInfixOf
          }

      main = do
        xmobarProc <- spawnPipe "${pkgs.xmobar}/bin/xmobar"
        xmonad $
          desktopConfig
            { borderWidth = 5,
              normalBorderColor = "#073642",
              focusedBorderColor = "#859900",
              layoutHook = myLayoutHook,
              logHook =
                dynamicLogWithPP
                  xmobarPP
                    { ppOutput = hPutStrLn xmobarProc,
                      ppCurrent = xmobarColor "#859900" "" . wrap "[" "]",
                      ppVisible = xmobarColor "#b58900" "" . wrap "(" ")",
                      ppHidden = xmobarColor "#93a1a1" "" . wrap "*" "",
                      ppSep = "<fc=#93a1a1> | </fc>",
                      ppLayout = xmobarColor "#93a1a1" "",
                      ppTitle = xmobarColor "#859900" "" . shorten 80
                    },
              manageHook = manageDocks,
              modMask = mod4Mask,
              terminal = "alacritty"
            }
            `additionalKeysP` myKeys
    '');
  };
}
