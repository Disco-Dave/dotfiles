module Main (main) where

import qualified Control.Concurrent.Async as Async
import qualified Data.Text as Text
import XMonad.Local.Environment (Environment (..), getEnvironment)
import qualified XMonad.Local.Hostname as Hostname
import XMonad.Local.Theme (Theme (Theme))
import qualified XMonad.Local.Theme as Theme
import qualified Xmobar

defaultConfig :: Theme -> Xmobar.Config
defaultConfig Theme{themeFont, themeXmobar} =
  Xmobar.defaultConfig
    { Xmobar.font = Text.unpack $ Theme.fontToXftText themeFont
    , Xmobar.additionalFonts = []
    , Xmobar.border = Xmobar.FullB
    , Xmobar.borderColor = Theme.colorToString $ Theme.xmobarBorder themeXmobar
    , Xmobar.bgColor = Theme.colorToString $ Theme.xmobarBackground themeXmobar
    , Xmobar.fgColor = Theme.colorToString $ Theme.xmobarForeground themeXmobar
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

primary :: Theme -> Xmobar.Config
primary theme =
  (defaultConfig theme)
    { Xmobar.position = Xmobar.OnScreen 0 Xmobar.Top
    , Xmobar.commands =
        [ Xmobar.Run $ Xmobar.Date "%a %b %_d %Y %I:%M:%S %p" "date" 10
        , Xmobar.Run Xmobar.UnsafeStdinReader
        , Xmobar.Run $ Xmobar.ComX "mpc" ["current", "-f", "%title% by %artist%"] "" "mpd" 10
        , Xmobar.Run $ Xmobar.Com "bash" ["/home/david/.config/xmonad/padding-icon.sh", "stalonetray"] "tray" 10
        ]
    , Xmobar.template = "%UnsafeStdinReader% } %date% { %mpd% %tray%"
    }

secondary :: Theme -> Xmobar.Config
secondary theme =
  (defaultConfig theme)
    { Xmobar.position = Xmobar.OnScreen 1 Xmobar.Top
    , Xmobar.commands =
        [ Xmobar.Run Xmobar.UnsafeStdinReader
        , Xmobar.Run $ Xmobar.ComX "curl" ["-s", "-G", "-d", "format=%C,%20%t", "wttr.in/17070"] "" "weather" 6000
        , Xmobar.Run $ Xmobar.Memory ["-t", "Mem: <usedratio>%"] 10
        , Xmobar.Run $ Xmobar.Swap [] 10
        , Xmobar.Run $ Xmobar.Cpu [] 10
        ]
    , Xmobar.template = "%UnsafeStdinReader% } %weather% { %cpu% | %memory% * %swap%"
    }

main :: IO ()
main = do
  env <- getEnvironment

  let spawn config =
        let theme = envTheme env
         in Xmobar.xmobar (config theme)

  let configs =
        case envHostname env of
          Hostname.Desktop -> [primary, secondary]
          _ -> [primary]

  Async.mapConcurrently_ spawn configs
