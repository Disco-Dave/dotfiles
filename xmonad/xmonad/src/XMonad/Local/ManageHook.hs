module XMonad.Local.ManageHook
  ( manageHook
  )
where

import Control.Monad.Reader (Reader)
import Shared.Environment (Environment)
import XMonad ((-->), (=?))
import XMonad qualified
import XMonad.Hooks.ManageHelpers qualified as ManageHelpers


manageHook :: Reader Environment XMonad.ManageHook
manageHook =
  pure . mconcat $ -- use xprop to find this information
    [ XMonad.className =? "Pavucontrol" --> ManageHelpers.doCenterFloat
    , XMonad.className =? "Gcolor3" --> ManageHelpers.doCenterFloat
    ]
