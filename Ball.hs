{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Ball
where
import Foreign.C (CInt)

import Graphics
import Util
import Constants

data Ball = Ball {
  bw :: CInt,
  bh :: CInt,
  bx :: CInt,
  by :: CInt,
  bvx :: CInt,
  bvy :: CInt,
  bmaxspeed :: CInt
} deriving Show

newBall :: Ball
newBall = Ball{
  bw = defballwidth,
  bh = defballheight,
  bx = defballx,
  by = defbally,
  bvx = defballspeed*speedup,
  bvy = defballspeed*speedup,
  bmaxspeed = 0
} 

ballDraw renderer Ball{..} = do
  fillCircle renderer (V2 bx by) (bw`div`2+2) white
