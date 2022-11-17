module Xmobar.Local (start) where

import Shared.Environment (Environment (..), getEnvironment)
import Shared.FilePaths qualified as FilePaths
import Shared.Hostname qualified as Hostname
import Shared.Theme qualified as Theme
import Shared.Theme.Color qualified as Color
import Shared.Theme.Font qualified as Font
import System.Environment (getArgs)
import System.FilePath qualified as FilePath
import Xmobar qualified


defaultConfig :: Environment -> Int -> Xmobar.Config
defaultConfig env screenId =
  let position =
        case env.hostname of
          Hostname.Laptop ->
            Xmobar.OnScreen screenId (Xmobar.TopH 22)
          _ ->
            Xmobar.OnScreen screenId (Xmobar.TopH 30)

      font =
        case env.hostname of
          Hostname.Laptop ->
            Font.toPangoString env.theme.font
          _ ->
            Font.toPangoString (env.theme.font{Font.size = 17})
   in Xmobar.defaultConfig
        { Xmobar.font = font
        , Xmobar.position = position
        , Xmobar.borderColor = Color.toHashString env.theme.xmobar.border
        , Xmobar.bgColor = Color.toHashString env.theme.xmobar.background
        , Xmobar.fgColor = Color.toHashString env.theme.xmobar.foreground
        , Xmobar.lowerOnStart = True
        , Xmobar.pickBroadest = False
        , Xmobar.persistent = False
        , Xmobar.hideOnStart = False
        , Xmobar.textOutputFormat = Xmobar.Pango
        , Xmobar.allDesktops = True
        , Xmobar.overrideRedirect = True
        }


primary :: Environment -> Xmobar.Config
primary env =
  let paddingIconScript =
        FilePath.joinPath
          [ env.filePaths.xdgConfig
          , "xmonad"
          , "xmobar"
          , "padding-icon.sh"
          ]

      config = defaultConfig env 0
   in case env.hostname of
        Hostname.Work ->
          config
            { Xmobar.commands =
                [ Xmobar.Run $ Xmobar.UnsafeXPropertyLog "_XMONAD_LOG_1"
                , Xmobar.Run $ Xmobar.Date "%a %b %_d %Y %I:%M:%S %p" "date" 10
                , Xmobar.Run $ Xmobar.Com "bash" [paddingIconScript, "panel"] "tray" 10
                ]
            , Xmobar.template = "%_XMONAD_LOG_1% } %date% { %tray%"
            }
        _ ->
          config
            { Xmobar.commands =
                [ Xmobar.Run $ Xmobar.UnsafeXPropertyLog "_XMONAD_LOG_1"
                , Xmobar.Run $ Xmobar.Date "%a %b %_d %Y %I:%M:%S %p" "date" 10
                , Xmobar.Run $ Xmobar.ComX "mpc" ["current", "-f", "%title% by %artist%"] "" "mpd" 10
                , Xmobar.Run $ Xmobar.Com "bash" [paddingIconScript, "panel"] "tray" 10
                ]
            , Xmobar.template = "%_XMONAD_LOG_1% } %date% { %mpd% %tray%"
            }


secondary :: Environment -> Xmobar.Config
secondary env =
  (defaultConfig env 2)
    { Xmobar.commands =
        [ Xmobar.Run $ Xmobar.UnsafeXPropertyLog "_XMONAD_LOG_2"
        ]
    , Xmobar.template = "%_XMONAD_LOG_2% } { "
    }


tertiary :: Environment -> Xmobar.Config
tertiary env =
  (defaultConfig env 1)
    { Xmobar.commands =
        [ Xmobar.Run $ Xmobar.UnsafeXPropertyLog "_XMONAD_LOG_3"
        , Xmobar.Run $ Xmobar.Memory ["-t", "Mem: <usedratio>%"] 10
        , Xmobar.Run $ Xmobar.Swap [] 10
        , Xmobar.Run $ Xmobar.Cpu [] 10
        ]
    , Xmobar.template = "%_XMONAD_LOG_3% } { %cpu% | %memory% * %swap%"
    }


getConfig :: IO (Environment -> Xmobar.Config)
getConfig = do
  args <- getArgs

  pure $ case args of
    ("--secondary" : _) -> secondary
    ("--tertiary" : _) -> tertiary
    _other -> primary


start :: IO ()
start = do
  env <- getEnvironment
  config <- getConfig
  Xmobar.xmobar (config env)
