module Theme where

data Theme color = Theme
  { windowFocused :: color
  , windowUnfocused :: color
  , xmobarBackground :: color
  , xmobarFont :: color
  , xmobarWorkspaceFocused :: color
  , xmobarWorkspaceName :: color
  , xmobarWindowName :: color
  }
  deriving (Show, Eq)
