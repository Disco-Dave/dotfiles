module Main where

import Config
import Config.Colors
import Config.Font
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Generics.Internal.VL.Lens
import Data.Generics.Product
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Yaml as Yaml
import qualified System.Environment as Env

defaultConfig :: Config
defaultConfig =
  Config
    { dynamicTitle = True,
      font =
        Font
          { size = 7.5,
            normal = FontFamily "Hack"
          },
      colors =
        Colors
          { primaryColors =
              PrimaryColors
                { primaryBackground = "0x2E3440",
                  primaryForeground = "0xD8DEE9"
                },
            normalColors =
              NormalColors
                { normalBlack = "0x3B4252",
                  normalRed = "0xBF616A",
                  normalGreen = "0xA3BE8C",
                  normalYellow = "0xEBCB8B",
                  normalBlue = "0x81A1C1",
                  normalMagenta = "0xB48EAD",
                  normalCyan = "0x88C0D0",
                  normalWhite = "0xE5E9F0"
                },
            brightColors =
              BrightColors
                { brightBlack = "0x4C566A",
                  brightRed = "0xBF616A",
                  brightGreen = "0xA3BE8C",
                  brightYellow = "0xEBCB8B",
                  brightBlue = "0x81A1C1",
                  brightMagenta = "0xB48EAD",
                  brightCyan = "0x8FBCBB",
                  brightWhite = "0xECEFF4"
                }
          }
    }

getConfig :: Bool -> Config
getConfig True = defaultConfig & field @"font" . field @"size" .~ 10
getConfig False = defaultConfig

main :: IO ()
main = do
  isDesktop <-
    Text.readFile "/etc/hostname"
      <&> Text.strip
      <&> (== "compe")
  let config = getConfig isDesktop
  home <- Env.getEnv "HOME"
  Yaml.encodeFile (home <> "/.config/alacritty/alacritty.yml") config
