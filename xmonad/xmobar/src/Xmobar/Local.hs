module Xmobar.Local (start) where

import Shared.Environment (Environment (..), getEnvironment)
import Shared.FilePaths qualified as FilePaths
import Shared.Hostname qualified as Hostname
import Shared.Theme (Theme)
import Shared.Theme qualified as Theme
import Shared.Theme.Color qualified as Color
import Shared.Theme.Font qualified as Font
import System.Environment (getArgs)
import System.FilePath qualified as FilePath
import Xmobar qualified


defaultConfig :: Theme -> Xmobar.Config
defaultConfig theme =
  Xmobar.defaultConfig
    { Xmobar.font = Font.toXftString theme.font
    , Xmobar.additionalFonts = []
    , Xmobar.border = Xmobar.FullB
    , Xmobar.borderColor = Color.toHashString theme.xmobar.border
    , Xmobar.bgColor = Color.toHashString theme.xmobar.background
    , Xmobar.fgColor = Color.toHashString theme.xmobar.foreground
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
primary env =
  let paddingIconScript =
        FilePath.joinPath
          [ env.filePaths.xdgConfig
          , "xmonad"
          , "padding-icon.sh"
          ]
   in case env.hostname of
        Hostname.Work ->
          (defaultConfig env.theme)
            { Xmobar.position = Xmobar.OnScreen 0 Xmobar.Top
            , Xmobar.commands =
                [ Xmobar.Run $ Xmobar.UnsafeXPropertyLog "_XMONAD_LOG_1"
                , Xmobar.Run $ Xmobar.Date "%a %b %_d %Y %I:%M:%S %p" "date" 10
                , Xmobar.Run $ Xmobar.Com "bash" [paddingIconScript, "panel"] "tray" 10
                ]
            , Xmobar.template = "%_XMONAD_LOG_1% } %date% { %tray%"
            }
        _ ->
          (defaultConfig env.theme)
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
secondary env =
  (defaultConfig env.theme)
    { Xmobar.position = Xmobar.OnScreen 2 Xmobar.Top
    , Xmobar.commands =
        [ Xmobar.Run $ Xmobar.UnsafeXPropertyLog "_XMONAD_LOG_2"
        , Xmobar.Run $ Xmobar.ComX "curl" ["-s", "-G", "-d", "format=%C,%20%t", "wttr.in/17070"] "" "weather" 6000
        ]
    , Xmobar.template = "%_XMONAD_LOG_2% } %weather% { "
    }


tertiary :: Environment -> Xmobar.Config
tertiary env =
  (defaultConfig env.theme)
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
    _ -> Xmobar.xmobar (primary env)
