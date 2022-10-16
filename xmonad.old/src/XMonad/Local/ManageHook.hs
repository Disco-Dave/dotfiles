module XMonad.Local.ManageHook (
  manageHook,
) where

import Control.Monad.Reader
import XMonad ((-->), (=?))
import qualified XMonad
import qualified XMonad.Hooks.ManageHelpers as ManageHelpers
import XMonad.Local.Environment

manageHook :: Reader Environment XMonad.ManageHook
manageHook =
  pure . mconcat $ -- use xprop to find this information
    [ XMonad.className =? "Pavucontrol" --> ManageHelpers.doCenterFloat
    ]
