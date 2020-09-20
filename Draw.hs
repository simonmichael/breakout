{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Draw
where
import SDL hiding (trace)
import SDL.Primitive

import Game
import Ball
import Bat

black = V4 0 0 0 255
red = V4 255 0 0 255
white = V4 255 255 255 255

gameDraw :: Game -> IO ()
gameDraw Game{grenderer,gbat,gball} = do
      rendererDrawColor grenderer $= black >> clear grenderer
      batDraw grenderer gbat
      ballDraw grenderer gball
      present grenderer

batDraw renderer Bat{..} = do
      fillRectangle renderer (V2 btx bty) (V2 (btx+btw) (bty+bth)) red

ballDraw renderer Ball{..} = do
      fillRectangle renderer (V2 bx by) (V2 (bx+bw) (by+bh)) white

