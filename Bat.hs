{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Bat
where
import Foreign.C (CInt)

import Util
import Types

-- a player bat
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

defbatfriction = 0.2
defbatmaxspeed = 5
defbataccel = 1
defbatwidth = 50
defbatheight = 10

newBat :: X -> Y -> Bat
newBat x y = Bat{
  btw = defbatwidth,
  bth = defbatheight,
  btx = x,
  bty = y,
  btvx = 0,
  btvy = 0,
  btmaxspeed = defbatmaxspeed,
  btaccel = defbataccel
}
