module MyXmobar (
  XmobarSettings,
  left,
  right,
  withXmobar,
  spawnLeft,
  spawnRight,
) where

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import Data.Foldable (for_)
import Data.Traversable (for)
import System.IO (Handle)
import System.Posix (forkProcess)
import System.Process (createPipe)
import qualified Xmobar

newtype XmobarSettings = XmobarSettings
  { fromXmobarSettings :: STM.TQueue String -> Xmobar.Config
  }

left :: XmobarSettings
left = XmobarSettings $ \queue ->
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
        , Xmobar.Run $ Xmobar.QueueReader queue id "reader"
        --, Xmobar.Run $ Xmobar.ComX "mpc" ["current", "-f", "%title% by %artist%"] "" "mpd" 10
        --, Xmobar.Run $ Xmobar.Com "bash" [paddingScriptPath, "stalonetray"] "tray" 10
        ]
    , Xmobar.sepChar = "%"
    , Xmobar.alignSep = "}{"
    , Xmobar.template = "%reader% } %date% { %mpd% %tray%"
    }

right :: XmobarSettings
right = XmobarSettings $ \queue ->
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
        [ Xmobar.Run $ Xmobar.QueueReader queue id "reader"
        , Xmobar.Run $ Xmobar.ComX "curl" ["-s", "-G", "-d", "format=%C,%20%t", "wttr.in/17070"] "" "weather" 6000
        , Xmobar.Run $ Xmobar.Memory ["-t", "Mem: <usedratio>%"] 10
        , Xmobar.Run $ Xmobar.Swap [] 10
        , Xmobar.Run $ Xmobar.Cpu [] 10
        ]
    , Xmobar.sepChar = "%"
    , Xmobar.alignSep = "}{"
    , Xmobar.template = "%UnsafeStdinReader% } %weather% { %cpu% | %memory% * %swap%"
    }

withXmobar :: [XmobarSettings] -> ((String -> IO ()) -> IO a) -> IO a
withXmobar settings useXmobar = do
  (queues, configs) <- fmap unzip . for settings $ \config -> do
    queue <- STM.newTQueueIO
    pure (queue, fromXmobarSettings config queue)

  let send message =
        STM.atomically . for_ queues $ \queue ->
          STM.writeTQueue queue message
      xmobars = Async.mapConcurrently_ Xmobar.xmobar configs

  Async.withAsync xmobars $ \x ->
    useXmobar send <* Async.cancel x

spawn :: (Handle -> Xmobar.Config) -> IO Handle
spawn config = do
  (readHandle, writeHandle) <- createPipe

  _ <- forkProcess . Xmobar.xmobar $ config readHandle
  pure writeHandle

spawnLeft :: IO Handle
spawnLeft =
  spawn $ \readHandle ->
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
          , Xmobar.Run $ Xmobar.HandleReader readHandle "reader"
          --, Xmobar.Run $ Xmobar.ComX "mpc" ["current", "-f", "%title% by %artist%"] "" "mpd" 10
          --, Xmobar.Run $ Xmobar.Com "bash" [paddingScriptPath, "stalonetray"] "tray" 10
          ]
      , Xmobar.sepChar = "%"
      , Xmobar.alignSep = "}{"
      , Xmobar.template = "%reader% } %date% { "
      }

spawnRight :: IO Handle
spawnRight =
  spawn $ \readHandle ->
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
          [ Xmobar.Run $ Xmobar.HandleReader readHandle "reader"
          , Xmobar.Run $ Xmobar.ComX "curl" ["-s", "-G", "-d", "format=%C,%20%t", "wttr.in/17070"] "" "weather" 6000
          , Xmobar.Run $ Xmobar.Memory ["-t", "Mem: <usedratio>%"] 10
          , Xmobar.Run $ Xmobar.Swap [] 10
          , Xmobar.Run $ Xmobar.Cpu [] 10
          ]
      , Xmobar.sepChar = "%"
      , Xmobar.alignSep = "}{"
      , Xmobar.template = "%reader% } %weather% { %cpu% | %memory% * %swap%"
      }
