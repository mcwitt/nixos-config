{ config, lib, pkgs, ... }:
{
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
