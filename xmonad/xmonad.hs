import           XMonad
import           XMonad.Config.Desktop


-- Set variables for mod masks, since I can never remeber them.
superKey :: KeyMask
superKey = mod4Mask

altKey :: KeyMask
altKey = mod1Mask


main :: IO ()
main = xmonad desktopConfig { terminal = "alacritty", modMask = mod1Mask }
