module XMonad.Local.Utils (
  currentScreenId,
  numberOfWindowsOnWorkspace,
) where

import qualified Control.Monad.State as State
import XMonad (X)
import qualified XMonad
import qualified XMonad.StackSet as StackSet

currentScreenId :: X Int
currentScreenId = do
  (XMonad.S screenId) <- State.gets $ StackSet.screen . StackSet.current . XMonad.windowset
  pure screenId

numberOfWindowsOnWorkspace :: X Int
numberOfWindowsOnWorkspace = do
  windowset <- XMonad.gets XMonad.windowset
  let workspace = StackSet.workspace $ StackSet.current windowset
      windowStack = StackSet.stack workspace
   in pure $ maybe 0 length windowStack
