module Main where

import Config
import Config.Colors
import Config.Font
import Config.Window
import qualified Data.Aeson as Aeson
import Data.Functor ((<&>))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified System.Environment as Env

defaultConfig :: Config
defaultConfig =
  Config
    { window = Window True,
      font =
        Font
          { size = 7.5,
            normal = FontFamily "Hack"
          },
      colors =
        Colors
          { primary =
              PrimaryColors
                { background = "0x2E3440",
                  foreground = "0xD8DEE9"
                },
            normal =
              NormalColors
                { black = "0x3B4252",
                  red = "0xBF616A",
                  green = "0xA3BE8C",
                  yellow = "0xEBCB8B",
                  blue = "0x81A1C1",
                  magenta = "0xB48EAD",
                  cyan = "0x88C0D0",
                  white = "0xE5E9F0"
                },
            bright =
              BrightColors
                { black = "0x4C566A",
                  red = "0xBF616A",
                  green = "0xA3BE8C",
                  yellow = "0xEBCB8B",
                  blue = "0x81A1C1",
                  magenta = "0xB48EAD",
                  cyan = "0x8FBCBB",
                  white = "0xECEFF4"
                }
          }
    }

getConfig :: Bool -> Config
getConfig False = defaultConfig
getConfig True =
  defaultConfig
    { font =
        (font defaultConfig)
          { size = 11
          }
    }

main :: IO ()
main = do
  isDesktop <-
    Text.readFile "/etc/hostname"
      <&> Text.strip
      <&> (== "compe")
  let config = getConfig isDesktop
  home <- Env.getEnv "HOME"
  Aeson.encodeFile (home <> "/.config/alacritty/alacritty.yml") config
