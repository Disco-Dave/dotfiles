import           Xmobar

config :: Config
config = defaultConfig
  { font             = "xft:FreeSans-12"
  , additionalFonts  = []
  , border           = FullB
  , borderColor      = "#3B4252"
  , bgColor          = "#3B4252"
  , fgColor          = "#ECEFF4"
  , alpha            = 255
  , position         = Top
  , textOffset       = -1
  , iconOffset       = -1
  , lowerOnStart     = True
  , pickBroadest     = False
  , persistent       = False
  , hideOnStart      = False
  , iconRoot         = "."
  , allDesktops      = True
  , overrideRedirect = True
  , commands         = [ Run $ Date "%a %b %_d %Y %I:%M:%S %p" "date" 10
                       , Run UnsafeStdinReader
                       ]
  , sepChar          = "%"
  , alignSep         = "}{"
  , template         = "%UnsafeStdinReader% } %date% { "
  }

main :: IO ()
main = xmobar config
