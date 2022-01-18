module MyXmobar (
  spawnLeft,
  spawnRight,
) where

import qualified Paths_xmonad_custom
import System.IO (Handle)
import System.Posix (forkProcess)
import System.Process (createPipe)
import qualified Xmobar

spawn :: Xmobar.Config -> IO Handle
spawn config = do
  (readHandle, writeHandle) <- createPipe

  _ <-
    forkProcess . Xmobar.xmobar $
      config
        { Xmobar.commands =
            let oldCommands = Xmobar.commands config
                handleReader = Xmobar.Run (Xmobar.HandleReader readHandle "handle")
             in handleReader : oldCommands
        }

  pure writeHandle

spawnLeft :: IO Handle
spawnLeft = do
  paddingScriptPath <- Paths_xmonad_custom.getDataFileName "data/padding-icon.sh"
  spawn
    Xmobar.defaultConfig
      { Xmobar.font = "xft:FreeSans-12"
      , Xmobar.additionalFonts = []
      , Xmobar.border = Xmobar.FullB
      , Xmobar.borderColor = "#3B4252"
      , Xmobar.bgColor = "#3B4252"
      , Xmobar.fgColor = "#ECEFF4"
      , Xmobar.alpha = 255
      , Xmobar.position = Xmobar.OnScreen 0 Xmobar.Top
      , Xmobar.textOffset = -1
      , Xmobar.iconOffset = -1
      , Xmobar.lowerOnStart = True
      , Xmobar.pickBroadest = False
      , Xmobar.persistent = False
      , Xmobar.hideOnStart = False
      , Xmobar.iconRoot = "."
      , Xmobar.allDesktops = True
      , Xmobar.overrideRedirect = True
      , Xmobar.commands =
          [ Xmobar.Run $ Xmobar.Date "%a %b %_d %Y %I:%M:%S %p" "date" 10
          , Xmobar.Run Xmobar.UnsafeStdinReader
          , Xmobar.Run $ Xmobar.ComX "mpc" ["current", "-f", "%title% by %artist%"] "" "mpd" 10
          , Xmobar.Run $ Xmobar.Com "bash" [paddingScriptPath, "stalonetray"] "tray" 10
          ]
      , Xmobar.sepChar = "%"
      , Xmobar.alignSep = "}{"
      , Xmobar.template = "%UnsafeStdinReader% } %date% { %mpd% %tray%"
      }

spawnRight :: IO Handle
spawnRight =
  spawn
    Xmobar.defaultConfig
      { Xmobar.font = "xft:FreeSans-12"
      , Xmobar.additionalFonts = []
      , Xmobar.border = Xmobar.FullB
      , Xmobar.borderColor = "#3B4252"
      , Xmobar.bgColor = "#3B4252"
      , Xmobar.fgColor = "#ECEFF4"
      , Xmobar.alpha = 255
      , Xmobar.position = Xmobar.OnScreen 1 Xmobar.Top
      , Xmobar.textOffset = -1
      , Xmobar.iconOffset = -1
      , Xmobar.lowerOnStart = True
      , Xmobar.pickBroadest = False
      , Xmobar.persistent = False
      , Xmobar.hideOnStart = False
      , Xmobar.iconRoot = "."
      , Xmobar.allDesktops = True
      , Xmobar.overrideRedirect = True
      , Xmobar.commands =
          [ Xmobar.Run Xmobar.UnsafeStdinReader
          , Xmobar.Run $ Xmobar.ComX "curl" ["-s", "-G", "-d", "format=%C,%20%t", "wttr.in/17070"] "" "weather" 6000
          , Xmobar.Run $ Xmobar.Memory ["-t", "Mem: <usedratio>%"] 10
          , Xmobar.Run $ Xmobar.Swap [] 10
          , Xmobar.Run $ Xmobar.Cpu [] 10
          ]
      , Xmobar.sepChar = "%"
      , Xmobar.alignSep = "}{"
      , Xmobar.template = "%UnsafeStdinReader% } %weather% { %cpu% | %memory% * %swap%"
      }
