module XMonad.Local.Utils (
  currentScreenId,
) where

import qualified Control.Monad.State as State
import XMonad (X)
import qualified XMonad
import qualified XMonad.StackSet as StackSet

currentScreenId :: X Int
currentScreenId = do
  (XMonad.S screenId) <-
    State.gets $
      StackSet.screen . StackSet.current . XMonad.windowset
  pure screenId
