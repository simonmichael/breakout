{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Ball
where
import Foreign.C (CInt)

import Util
import Types

-- a ball
data Ball = Ball {
  bw :: CInt,
  bh :: CInt,
  bx :: CInt,
  by :: CInt,
  bvx :: CInt,
  bvy :: CInt,
  bmaxspeed :: CInt
}

defballspeed = 4
defballwidth = 8
defballheight = 8

newBall :: Ball
newBall = Ball{
  bw = defballwidth,
  bh = defballheight,
  bx = 0,
  by = 0,
  bvx = defballspeed,
  bvy = defballspeed,
  bmaxspeed = 0
} 
