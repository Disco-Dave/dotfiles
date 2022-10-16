-- https://www.nordtheme.com/docs/colors-and-palettes
module Shared.Theme.Palettes.Nord
  ( polarNight0
  , polarNight1
  , polarNight2
  , polarNight3
  , snowStorm0
  , snowStorm1
  , snowStorm2
  , frost0
  , frost1
  , frost2
  , frost3
  , auroraRed
  , auroraOrange
  , auroraYellow
  , auroraGreen
  , auroraPurple
  )
where

import Shared.Theme.Color (Color (..), HexDigit (..))


polarNight0 :: Color
polarNight0 =
  -- "#2E3440"
  Color (HexDigit2, HexDigitE) (HexDigit3, HexDigit4) (HexDigit4, HexDigit0)


polarNight1 :: Color
polarNight1 =
  -- "#3B4252"
  Color (HexDigit3, HexDigitB) (HexDigit4, HexDigit2) (HexDigit5, HexDigit2)


polarNight2 :: Color
polarNight2 =
  -- "#434C5E"
  Color (HexDigit4, HexDigit3) (HexDigit4, HexDigitC) (HexDigit5, HexDigitE)


polarNight3 :: Color
polarNight3 =
  -- "#4C566A"
  Color (HexDigit4, HexDigitC) (HexDigit5, HexDigit6) (HexDigit6, HexDigitA)


snowStorm0 :: Color
snowStorm0 =
  -- "#D8DEE9"
  Color (HexDigitD, HexDigit8) (HexDigitD, HexDigitE) (HexDigitE, HexDigit9)


snowStorm1 :: Color
snowStorm1 =
  -- "#E5E9F0"
  Color (HexDigitE, HexDigit5) (HexDigitE, HexDigit9) (HexDigitF, HexDigit0)


snowStorm2 :: Color
snowStorm2 =
  -- "#ECEFF4"
  Color (HexDigitE, HexDigitC) (HexDigitE, HexDigitF) (HexDigitF, HexDigit4)


frost0 :: Color
frost0 =
  -- "#8FBCBB"
  Color (HexDigit8, HexDigitF) (HexDigitB, HexDigitC) (HexDigitB, HexDigitB)


frost1 :: Color
frost1 =
  -- "#88C0D0"
  Color (HexDigit8, HexDigit8) (HexDigitC, HexDigit0) (HexDigitD, HexDigit0)


frost2 :: Color
frost2 =
  -- "#81A1C1"
  Color (HexDigit8, HexDigit1) (HexDigitA, HexDigit1) (HexDigitC, HexDigit1)


frost3 :: Color
frost3 =
  -- "#5E81AC"
  Color (HexDigit5, HexDigitE) (HexDigit8, HexDigit1) (HexDigitA, HexDigitC)


auroraRed :: Color
auroraRed =
  -- "#BF616A"
  Color (HexDigitB, HexDigitF) (HexDigit6, HexDigit1) (HexDigit6, HexDigitA)


auroraOrange :: Color
auroraOrange =
  -- "#D08770"
  Color (HexDigitD, HexDigit0) (HexDigit8, HexDigit7) (HexDigit7, HexDigit0)


auroraYellow :: Color
auroraYellow =
  -- "#EBCB8B"
  Color (HexDigitE, HexDigitB) (HexDigitC, HexDigitB) (HexDigit8, HexDigitB)


auroraGreen :: Color
auroraGreen =
  -- "#A3BE8C"
  Color (HexDigitA, HexDigit3) (HexDigitB, HexDigitE) (HexDigit8, HexDigitC)


auroraPurple :: Color
auroraPurple =
  -- "#B48EAD"
  Color (HexDigitB, HexDigit4) (HexDigit8, HexDigitE) (HexDigitA, HexDigitD)
