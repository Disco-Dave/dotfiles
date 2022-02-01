module Colors.Nord (
  polarNight0,
  polarNight1,
  polarNight2,
  polarNight3,
  snowStorm0,
  snowStorm1,
  snowStorm2,
  frost0,
  frost1,
  frost2,
  frost3,
  auroraRed,
  auroraOrange,
  auroraYellow,
  auroraGreen,
  auroraPurple,
) where

import Data.String (IsString)

polarNight0 :: IsString color => color
polarNight0 = "#2E3440"

polarNight1 :: IsString color => color
polarNight1 = "#3B4252"

polarNight2 :: IsString color => color
polarNight2 = "#434C5E"

polarNight3 :: IsString color => color
polarNight3 = "#4C566A"

snowStorm0 :: IsString color => color
snowStorm0 = "#D8DEE9"

snowStorm1 :: IsString color => color
snowStorm1 = "#E5E9F0"

snowStorm2 :: IsString color => color
snowStorm2 = "#ECEFF4"

frost0 :: IsString color => color
frost0 = "#8FBCBB"

frost1 :: IsString color => color
frost1 = "#88C0D0"

frost2 :: IsString color => color
frost2 = "#81A1C1"

frost3 :: IsString color => color
frost3 = "#5E81AC"

auroraRed :: IsString color => color
auroraRed = "#BF616A"

auroraOrange :: IsString color => color
auroraOrange = "#D08770"

auroraYellow :: IsString color => color
auroraYellow = "#EBCB8B"

auroraGreen :: IsString color => color
auroraGreen = "#A3BE8C"

auroraPurple :: IsString color => color
auroraPurple = "#B48EAD"
