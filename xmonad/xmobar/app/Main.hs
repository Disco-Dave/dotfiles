module Main (main) where

import Xmobar.Local qualified

main :: IO ()
main =
  Xmobar.Local.start
