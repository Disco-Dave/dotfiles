import Xmobar

config :: Config
config =
  defaultConfig
    { font = "xft:FreeSans-12",
      additionalFonts = [],
      border = FullB,
      borderColor = "#3B4252",
      bgColor = "#3B4252",
      fgColor = "#ECEFF4",
      alpha = 255,
      position = OnScreen 1 Top,
      textOffset = -1,
      iconOffset = -1,
      lowerOnStart = True,
      pickBroadest = False,
      persistent = False,
      hideOnStart = False,
      iconRoot = ".",
      allDesktops = True,
      overrideRedirect = True,
      commands =
        [ Run UnsafeStdinReader,
          Run $ Com "curl" ["-s", "-G", "-d", "format=%C,%20%t", "wttr.in/17070"] "weather" 10000
        ],
      sepChar = "%",
      alignSep = "}{",
      template = "%UnsafeStdinReader% } %weather% {"
    }

main :: IO ()
main = xmobar config
