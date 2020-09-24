{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Bat
where
import Foreign.C (CInt)

import Graphics
import Util
import Constants

data Bat = Bat {
  btw :: CInt,
  bth :: CInt,
  btx :: CInt,
  bty :: CInt,
  btvx :: CInt,
  btvy :: CInt,
  btmaxspeed :: CInt,
  btaccel :: CInt
}

-- A new bat centered near the bottom of the given window dimensions.
newBat :: X -> Y -> Bat
newBat windoww windowh = Bat{
  btw = defbatwidth,
  bth = defbatheight,
  btx = windoww `div` 2,
  bty = windowh - defbatheight - 40,
  btvx = 0,
  btvy = 0,
  btmaxspeed = defbatmaxspeed*speedup,
  btaccel = defbataccel*speedup
}

batDraw renderer Bat{..} = do
  fillRectangle renderer (V2 btx bty) (V2 (btx+btw) (bty+bth)) red
