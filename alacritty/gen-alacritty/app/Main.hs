module Main where

import Config
import Config.Colors
import Config.Font
import Config.Window
import qualified Data.Aeson as Aeson
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
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
  let config =
        Config
          { window = Window True,
            font =
              Font
                { size = 11,
                  normal = FontFamily "Hack"
                },
            colors = dark
          }
      applyTheme Dark c = c {colors = dark}
      applyTheme Light c = c {colors = light}
      applyComputer Desktop c = c {font = (font c) {size = 11}}
      applyComputer Laptop c = c {font = (font c) {size = 7.5}}
   in config & applyTheme theme & applyComputer computer

writeConfig :: Config -> IO ()
writeConfig config = do
  home <- Env.getEnv "HOME"
  Aeson.encodeFile (home <> "/.config/alacritty/alacritty.yml") config

main :: IO ()
main = getSettings <&> makeConfig >>= writeConfig
