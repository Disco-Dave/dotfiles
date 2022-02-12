module XMonad.Local.ManageHook (
  manageHook,
) where

import Control.Monad.Reader (ReaderT)
import XMonad (X, (-->), (=?))
import qualified XMonad
import qualified XMonad.Hooks.ManageHelpers as ManageHelpers
import XMonad.Local.Environment (Environment)

manageHook :: ReaderT Environment X XMonad.ManageHook
manageHook =
  pure . mconcat $ -- use xprop to find this information
    [ XMonad.className =? "Pavucontrol" --> ManageHelpers.doCenterFloat
    ]
