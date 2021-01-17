{ pkgs, ... }:
{
  home.packages = [ pkgs.xmobar ];

  xdg.configFile.xmobar.text = ''
    Config {

       -- appearance
         font =         "xft:Bitstream Vera Sans Mono:size=10:bold:antialias=true"
       , bgColor =      "#002b36"
       , fgColor =      "#839496"
       , position =     Top
       , border =       BottomB
       , borderColor =  "#839496"

       -- layout
       , sepChar =  "%"   -- delineator between plugin names and straight text
       , alignSep = "}{"  -- separator between left-right alignment
       , template = "%StdinReader% | %multicpu% | %coretemp% | %memory% | %dynnetwork% }{ %KSFO% | %date% "

       -- general behavior
       , lowerOnStart =     True    -- send to bottom of window stack on start
       , hideOnStart =      False   -- start with window unmapped (hidden)
       , allDesktops =      True    -- show on all desktops
       , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
       , pickBroadest =     False   -- choose widest display (multi-monitor)
       , persistent =       True    -- enable/disable hiding (True = disabled)

       -- plugins
       --   Numbers can be automatically colored according to their value. xmobar
       --   decides color based on a three-tier/two-cutoff system, controlled by
       --   command options:
       --     --Low sets the low cutoff
       --     --High sets the high cutoff
       --
       --     --low sets the color below --Low cutoff
       --     --normal sets the color between --Low and --High cutoffs
       --     --High sets the color above --High cutoff
       --
       --   The --template option controls how the plugin is displayed. Text
       --   color can be set by enclosing in <fc></fc> tags. For more details
       --   see http://projects.haskell.org/xmobar/#system-monitor-plugins.
       , commands =

            -- weather monitor
            [ Run Weather "KSFO" [ "--template", "<skyCondition> | <fc=#268bd2><tempF></fc>°F | <fc=#268bd2><rh></fc>% | <fc=#268bd2><pressure></fc>hPa"
                                 ] 36000

            -- network activity monitor (dynamic interface resolution)
            , Run DynNetwork     [ "--template" , "<dev>: <tx>kB/s|<rx>kB/s"
                                 , "--Low"      , "1000"       -- units: B/s
                                 , "--High"     , "5000"       -- units: B/s
                                 , "--low"      , "#859900"
                                 , "--normal"   , "#b58900"
                                 , "--high"     , "#dc322f"
                                 ] 10

            -- cpu activity monitor
            , Run MultiCpu       [ "--template" , "Cpu: <total0>% <total1>% <total2>% <total3>% <total4>% <total5>% <total6>% <total7>%"
                                 , "--Low"      , "50"         -- units: %
                                 , "--High"     , "85"         -- units: %
                                 , "--low"      , "#859900"
                                 , "--normal"   , "#b58900"
                                 , "--high"     , "#dc322f"
                                 , "--ppad"     , "3"
                                 ] 10

            -- cpu core temperature monitor
            , Run CoreTemp       [ "--template" , "Temp: <core0>°C <core1>°C <core2>°C <core3>°C"
                                 , "--Low"      , "70"        -- units: °C
                                 , "--High"     , "80"        -- units: °C
                                 , "--low"      , "#859900"
                                 , "--normal"   , "#b58900"
                                 , "--high"     , "#dc322f"
                                 ] 50

            -- memory usage monitor
            , Run Memory         [ "--template" ,"Mem: <usedratio>%"
                                 , "--Low"      , "20"        -- units: %
                                 , "--High"     , "90"        -- units: %
                                 , "--low"      , "#859900"
                                 , "--normal"   , "#b58900"
                                 , "--high"     , "#dc322f"
                                 ] 10

            -- time and date indicator
            --   (%F = y-m-d date, %a = day of week, %T = h:m:s time)
            , Run Date           "<fc=#93a1a1>%F (%a) %T</fc>" "date" 10
            , Run StdinReader
            ]
       }
  '';
}
