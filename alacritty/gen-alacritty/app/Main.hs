module Main (main) where

import Config (Config (..))
import Config.Colors (dark, light)
import Config.Font (Font (..), FontFamily (..))
import Config.Window (Window (..))
import Data.Functor ((<&>))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Yaml as Yaml
import qualified System.Environment as Env

data Computer
  = Desktop
  | Laptop

data Theme
  = Dark
  | Light

data Settings = Settings
  { computer :: Computer,
    theme :: Theme
  }

getSettings :: IO Settings
getSettings = do
  computer <- do
    isDesktop <-
      Text.readFile "/etc/hostname"
        <&> Text.strip
        <&> (== "compe")
    pure $ if isDesktop then Desktop else Laptop
  theme <- do
    isLight <- any (== "--light") <$> Env.getArgs
    pure $ if isLight then Light else Dark
  pure $ Settings {..}

makeConfig :: Settings -> Config
makeConfig Settings {..} =
  let defaultConfig =
        Config
          { window = Window True,
            font =
              Font
                { size = 11,
                  normal = FontFamily "Hack"
                },
            colors = dark
          }
      applySettings =
        let adjustColors config =
              case theme of
                Dark -> config {colors = dark}
                Light -> config {colors = light}
            adjustFont config =
              case computer of
                Desktop -> config {font = (font config) {size = 11}}
                Laptop -> config {font = (font config) {size = 7.5}}
         in adjustColors . adjustFont
   in applySettings defaultConfig

writeConfig :: Config -> IO ()
writeConfig config = do
  home <- Env.getEnv "HOME"
  let path = home <> "/.config/alacritty/alacritty.yml"
  Yaml.encodeFile path config

main :: IO ()
main =
  getSettings
    <&> makeConfig
    >>= writeConfig
