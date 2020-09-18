{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Bat
where
import Foreign.C (CInt)

import Util
import Types

-- a player bat
data Bat = Bat {
      batw :: CInt,
      bath :: CInt,
      batx :: CInt,
      baty :: CInt,
      batvx :: CInt,
      batvy :: CInt,
      batmaxspeed :: CInt,
      bataccel :: CInt
}

defbatfriction = 0.2
defbatmaxspeed = 5
defbataccel = 1
defbatwidth = 50
defbatheight = 10

newBat :: X -> Y -> Bat
newBat x y = Bat{
  batw = defbatwidth,
  bath = defbatheight,
  batx = x,
  baty = y,
  batvx = 0,
  batvy = 0,
  batmaxspeed = defbatmaxspeed,
  bataccel = defbataccel
}
