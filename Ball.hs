{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Ball
where
import Foreign.C (CInt)

import Util
import Types

-- a ball
data Ball = Ball {
      ballw :: CInt,
      ballh :: CInt,
      ballx :: CInt,
      bally :: CInt,
      ballvx :: CInt,
      ballvy :: CInt,
      ballmaxspeed :: CInt
}

defballspeed = 4
defballwidth = 8
defballheight = 8

newBall :: Ball
newBall = Ball{
  ballw = defballwidth,
  ballh = defballheight,
  ballx = 0,
  bally = 0,
  ballvx = defballspeed,
  ballvy = defballspeed,
  ballmaxspeed = 0
} 
