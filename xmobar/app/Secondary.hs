module Main (main) where

import Xmobar

config :: Config
config =
  defaultConfig
    { font = "xft:FreeSans-12"
    , additionalFonts = []
    , border = FullB
    , borderColor = "#3B4252"
    , bgColor = "#3B4252"
    , fgColor = "#ECEFF4"
    , alpha = 255
    , position = OnScreen 1 Top
    , textOffset = -1
    , iconOffset = -1
    , lowerOnStart = True
    , pickBroadest = False
    , persistent = False
    , hideOnStart = False
    , iconRoot = "."
    , allDesktops = True
    , overrideRedirect = True
    , commands =
        [ Run UnsafeStdinReader
        , Run $ ComX "curl" ["-s", "-G", "-d", "format=%C,%20%t", "wttr.in/17070"] "" "weather" 6000
        , Run $ Memory ["-t", "Mem: <usedratio>%"] 10
        , Run $ Swap [] 10
        , Run $ Cpu [] 10
        ]
    , sepChar = "%"
    , alignSep = "}{"
    , template = "%UnsafeStdinReader% } %weather% { %cpu% | %memory% * %swap%"
    }

main :: IO ()
main = xmobar config
