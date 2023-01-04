{ config, lib, pkgs, ... }:
{
  programs.xmobar = let s = config.scheme.withHashtag; in {
    enable = true;
    rc.commands = [
      ''
        Run DynNetwork [ "--template" , "<dev>: <tx>kB/s|<rx>kB/s"
                       , "--Low"      , "1000"
                       , "--High"     , "5000"
                       , "--low"      , "${s.green}"
                       , "--normal"   , "${s.yellow}"
                       , "--high"     , "${s.red}"
                       ] 10
      ''
      ''
        Run MultiCpu   [ "--template" , "Cpu: <total0>% <total1>% <total2>% <total3>% <total4>% <total5>% <total6>% <total7>%"
                       , "--Low"      , "50"
                       , "--High"     , "85"
                       , "--low"      , "${s.green}"
                       , "--normal"   , "${s.yellow}"
                       , "--high"     , "${s.red}"
                       , "--ppad"     , "3"
                       ] 10
      ''
      ''
        Run CoreTemp   [ "--template" , "Temp: <core0>°C <core1>°C <core2>°C <core3>°C"
                       , "--Low"      , "70"
                       , "--High"     , "80"
                       , "--low"      , "${s.green}"
                       , "--normal"   , "${s.yellow}"
                       , "--high"     , "${s.red}"
                       ] 50
      ''
      ''
        Run Memory     [ "--template" ,"Mem: <usedratio>%"
                       , "--Low"      , "20"
                       , "--High"     , "90"
                       , "--low"      , "${s.green}"
                       , "--normal"   , "${s.yellow}"
                       , "--high"     , "${s.red}"
                       ] 10
      ''
      ''Run Date "<fc=${s.base05}>%F (%a) %T</fc>" "date" 10''
      ''Run StdinReader''
    ];
    rc.extraConfig = let inherit (lib) mkDefault; in {
      position = mkDefault "Top";
      font = mkDefault ''"Iosevka Comfy 20"''; # HACK: doesn't scale on hidpi
      template = mkDefault ''"<fc=${s.green}>%StdinReader%</fc> | %multicpu% | %coretemp% | %memory% | %dynnetwork% }{ %date% "'';
      bgColor = ''"${s.base00}"'';
      fgColor = ''"${s.base05}"'';
      borderColor = ''"${s.base00}"'';
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
    config = let s = config.scheme.withHashtag; in pkgs.writeText "xmonad.hs" ''
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
      import XMonad.Layout.NoBorders (smartBorders)
      import XMonad.Layout.Reflect (REFLECTX (REFLECTX))
      import XMonad.Prompt
      import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)
      import XMonad.Util.Run (hPutStrLn, spawnPipe)

      myLayoutHook =
        let tall = Tall 1 (1 % 50) (3 % 5)
            layouts = tall ||| Mirror tall ||| Full
            minimizeBoring = minimize . boringWindows
         in avoidStruts
              . mkToggle (single REFLECTX)
              . smartBorders
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

      main = do
        xmobarProc <- spawnPipe "xmobar"
        xmonad
          . docks
          . ewmh
          $ def
            { borderWidth = 5,
              normalBorderColor = "${s.base02}",
              focusedBorderColor = "${s.magenta}",
              layoutHook = myLayoutHook,
              logHook =
                dynamicLogWithPP
                  xmobarPP
                    { ppOutput = hPutStrLn xmobarProc,
                      ppCurrent = xmobarColor "${s.green}" "" . wrap "[" "]",
                      ppVisible = xmobarColor "${s.yellow}" "" . wrap "(" ")",
                      ppHidden = xmobarColor "${s.base02}" "" . wrap "*" "",
                      ppSep = "<fc=${s.base05}> | </fc>",
                      ppLayout = xmobarColor "${s.base05}" "",
                      ppTitle = xmobarColor "${s.green}" "" . shorten 80
                    },
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
                               ((mod4Mask, xK_g), spawn "rofi -show window"),
                               ((mod4Mask, xK_p), spawn "rofi -show drun"),
                               ((mod4Mask + shiftMask, xK_p), spawn "rofi -show run"),
                               ((mod4Mask, xK_o), spawn "rofi-pass"),
                               ((mod4Mask, xK_i), spawn "rofi -show ssh"),
                               ((mod4Mask, xK_semicolon), spawn "rofimoji --files emojis general_punctuation math"),
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
