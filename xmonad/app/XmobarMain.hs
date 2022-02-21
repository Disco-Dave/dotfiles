module Main (main) where

import System.Environment (getArgs)
import qualified System.FilePath as FilePath
import XMonad.Local.Environment (Environment (..), getEnvironment)
import qualified XMonad.Local.FilePaths as FilePaths
import XMonad.Local.Theme (Theme (Theme))
import qualified XMonad.Local.Theme as Theme
import qualified XMonad.Local.Theme.Color as Color
import qualified XMonad.Local.Theme.Font as Font
import qualified Xmobar

defaultConfig :: Theme -> Xmobar.Config
defaultConfig Theme{themeFont, themeXmobar} =
  Xmobar.defaultConfig
    { Xmobar.font = Font.toXftString themeFont
    , Xmobar.additionalFonts = []
    , Xmobar.border = Xmobar.FullB
    , Xmobar.borderColor = Color.toHashString $ Theme.xmobarBorder themeXmobar
    , Xmobar.bgColor = Color.toHashString $ Theme.xmobarBackground themeXmobar
    , Xmobar.fgColor = Color.toHashString $ Theme.xmobarForeground themeXmobar
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
          [ FilePaths.xdgConfig $ envFilePaths env
          , "xmonad"
          , "padding-icon.sh"
          ]
   in (defaultConfig (envTheme env))
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
  (defaultConfig (envTheme env))
    { Xmobar.position = Xmobar.OnScreen 1 Xmobar.Top
    , Xmobar.commands =
        [ Xmobar.Run $ Xmobar.UnsafeXPropertyLog "_XMONAD_LOG_2"
        , Xmobar.Run $ Xmobar.ComX "curl" ["-s", "-G", "-d", "format=%C,%20%t", "wttr.in/17070"] "" "weather" 6000
        , Xmobar.Run $ Xmobar.Memory ["-t", "Mem: <usedratio>%"] 10
        , Xmobar.Run $ Xmobar.Swap [] 10
        , Xmobar.Run $ Xmobar.Cpu [] 10
        ]
    , Xmobar.template = "%_XMONAD_LOG_2% } %weather% { %cpu% | %memory% * %swap%"
    }

main :: IO ()
main = do
  env <- getEnvironment

  args <- getArgs

  case args of
    ("--secondary" : _) -> Xmobar.xmobar (secondary env)
    _ -> Xmobar.xmobar (primary env)
