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
gameDraw Game{renderer,bat,ball} = do
      rendererDrawColor renderer $= black >> clear renderer
      batDraw renderer bat
      ballDraw renderer ball
      present renderer

batDraw renderer Bat{..} = do
      fillRectangle renderer (V2 batx baty) (V2 (batx+batw) (baty+bath)) red

ballDraw renderer Ball{..} = do
      fillRectangle renderer (V2 ballx bally) (V2 (ballx+ballw) (bally+ballh)) white

