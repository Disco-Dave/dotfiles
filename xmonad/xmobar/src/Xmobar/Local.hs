module Xmobar.Local (start) where

import Shared.Environment (Environment (..), getEnvironment)
import qualified Shared.FilePaths as FilePaths
import qualified Shared.Hostname as Hostname
import Shared.Theme (Theme (Theme))
import qualified Shared.Theme as Theme
import qualified Shared.Theme.Color as Color
import qualified Shared.Theme.Font as Font
import System.Environment (getArgs)
import qualified System.FilePath as FilePath
import qualified Xmobar


defaultConfig :: Theme -> Xmobar.Config
defaultConfig Theme{..} =
  let color selector =
        Color.toHashString (selector xmobar)
   in Xmobar.defaultConfig
        { Xmobar.font = Font.toXftString font
        , Xmobar.additionalFonts = []
        , Xmobar.border = Xmobar.FullB
        , Xmobar.borderColor = color Theme.border
        , Xmobar.bgColor = color Theme.background
        , Xmobar.fgColor = color Theme.foreground
        , Xmobar.alpha = 255
        , Xmobar.textOffset = -1
        , Xmobar.iconOffset = -1
        , Xmobar.lowerOnStart = True
        , Xmobar.pickBroadest = False
        , Xmobar.persistent = False
        , Xmobar.hideOnStart = False
        , Xmobar.iconRoot = "."
        , Xmobar.allDesktops = True
        , Xmobar.overrideRedirect = True
        , Xmobar.sepChar = "%"
        , Xmobar.alignSep = "}{"
        }


primary :: Environment -> Xmobar.Config
primary Environment{..} =
  let paddingIconScript =
        FilePath.joinPath
          [ FilePaths.xdgConfig filePaths
          , "xmonad"
          , "xmobar"
          , "padding-icon.sh"
          ]
   in case hostname of
        Hostname.Work ->
          (defaultConfig theme)
            { Xmobar.position = Xmobar.OnScreen 0 Xmobar.Top
            , Xmobar.commands =
                [ Xmobar.Run $ Xmobar.UnsafeXPropertyLog "_XMONAD_LOG_1"
                , Xmobar.Run $ Xmobar.Date "%a %b %_d %Y %I:%M:%S %p" "date" 10
                , Xmobar.Run $ Xmobar.Com "bash" [paddingIconScript, "panel"] "tray" 10
                ]
            , Xmobar.template = "%_XMONAD_LOG_1% } %date% { %tray%"
            }
        _other ->
          (defaultConfig theme)
            { Xmobar.position = Xmobar.OnScreen 0 Xmobar.Top
            , Xmobar.commands =
                [ Xmobar.Run $ Xmobar.UnsafeXPropertyLog "_XMONAD_LOG_1"
                , Xmobar.Run $ Xmobar.Date "%a %b %_d %Y %I:%M:%S %p" "date" 10
                , Xmobar.Run $ Xmobar.ComX "mpc" ["current", "-f", "%title% by %artist%"] "" "mpd" 10
                , Xmobar.Run $ Xmobar.Com "bash" [paddingIconScript, "panel"] "tray" 10
                ]
            , Xmobar.template = "%_XMONAD_LOG_1% } %date% { %mpd% %tray%"
            }


secondary :: Environment -> Xmobar.Config
secondary Environment{..} =
  (defaultConfig theme)
    { Xmobar.position = Xmobar.OnScreen 2 Xmobar.Top
    , Xmobar.commands =
        [ Xmobar.Run $ Xmobar.UnsafeXPropertyLog "_XMONAD_LOG_2"
        , Xmobar.Run $ Xmobar.ComX "curl" ["-s", "-G", "-d", "format=%C,%20%t", "wttr.in/17070"] "" "weather" 6000
        ]
    , Xmobar.template = "%_XMONAD_LOG_2% } %weather% { "
    }


tertiary :: Environment -> Xmobar.Config
tertiary Environment{..} =
  (defaultConfig theme)
    { Xmobar.position = Xmobar.OnScreen 1 Xmobar.Top
    , Xmobar.commands =
        [ Xmobar.Run $ Xmobar.UnsafeXPropertyLog "_XMONAD_LOG_3"
        , Xmobar.Run $ Xmobar.Memory ["-t", "Mem: <usedratio>%"] 10
        , Xmobar.Run $ Xmobar.Swap [] 10
        , Xmobar.Run $ Xmobar.Cpu [] 10
        ]
    , Xmobar.template = "%_XMONAD_LOG_3% } { %cpu% | %memory% * %swap%"
    }


start :: IO ()
start = do
  env <- getEnvironment

  args <- getArgs

  case args of
    ("--secondary" : _) -> Xmobar.xmobar (secondary env)
    ("--tertiary" : _) -> Xmobar.xmobar (tertiary env)
    _other -> Xmobar.xmobar (primary env)
