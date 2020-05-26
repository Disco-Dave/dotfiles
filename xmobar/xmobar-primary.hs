import Xmobar

config :: Config
config = defaultConfig 
  { font = "xft:Sans Mono-12"
  , additionalFonts = []
  , borderColor = "black"
  , border = FullB
  , bgColor = "black"
  , fgColor = "grey"
  , alpha = 255
  , position = Top
  , textOffset = -1
  , iconOffset = -1
  , lowerOnStart = True
  , pickBroadest = False
  , persistent = False
  , hideOnStart = False
  , iconRoot = "."
  , allDesktops = True
  , overrideRedirect = True
  , commands = [ Run $ Date "%a %b %_d %Y %I:%M:%S %p" "date" 10
               , Run StdinReader
               ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "%StdinReader% } %date% { "
  }

main :: IO ()
main = xmobar config
