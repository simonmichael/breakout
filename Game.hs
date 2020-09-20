{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Game
where
import Foreign.C (CInt)
import SDL hiding (trace)
import SDL.Framerate (Manager)

import Util
import Types
import Bat
import Ball

-- app, window, game state
data Game = Game {
  gwindow :: Window,
  grenderer :: Renderer,
  gfpsmgr :: Manager,
  gw :: CInt,
  gh :: CInt,
  gleftPressed :: Bool,
  grightPressed :: Bool,
  gbat :: Bat,
  gball :: Ball
}

newGame :: Window -> Renderer -> Manager -> CInt -> CInt -> Game
newGame window renderer fpsmgr width height = Game {
  gwindow = window,
  grenderer = renderer,
  gfpsmgr = fpsmgr,
  gw = width,
  gh = height,
  gleftPressed = False,
  grightPressed = False,
  gbat = newBat (div width 2) (height-defbatheight-40),
  gball = newBall
}

gameHandleEvent :: Game -> Event -> IO Game
gameHandleEvent game event =
  -- mtrace "evdata" evdata
  -- mtrace "keycode" $ keysymKeycode keyboardEventKeysym
  case event of
    _
      --  | event `isKeyDn` KeycodeQ -> gameExit game >> return game
      | event `isKeyDn` KeycodeLeft -> return game{gleftPressed=True}
      | event `isKeyUp` KeycodeLeft -> return game{gleftPressed=False}
      | event `isKeyDn` KeycodeRight -> return game{grightPressed=True}
      | event `isKeyUp` KeycodeRight -> return game{grightPressed=False}
      | otherwise -> return game

gameStep :: Game -> Game
gameStep game@Game{gw, gh, gleftPressed, grightPressed, gbat=bat@Bat{..}, gball=ball@Ball{..}} =
  game{gbat=bat', gball=ball'}
  where
    bat' = gameStepBat game bat
    ball' = gameStepBall game ball

gameStepBat :: Game -> Bat -> Bat
gameStepBat game@Game{gw, gh, gleftPressed, grightPressed} bat@Bat{..} = bat'
  where
    btvx' = if gleftPressed then (max (btvx-btaccel) (-btmaxspeed)) else btvx
    btvx'' = if grightPressed then (min (btvx'+btaccel) (btmaxspeed)) else btvx'
    btvx''' = if (and [not gleftPressed, not grightPressed]) then truncate(fromIntegral btvx'' * (1.0-defbatfriction)) else btvx''
    (btx',btvx'''') = incrementWithStop btx  btvx''' 0 (gw-btw)
    (bty',btvy') = (gh-bth-40, 0)
    bat' = bat{btx=btx',bty=bty',btvx=btvx''',btvy=btvy'}

gameStepBall :: Game -> Ball -> Ball
gameStepBall game@Game{gw, gh, gleftPressed, grightPressed, gbat=Bat{..}} ball@Ball{..} = ball'
  where
    (bx',bvx') = incrementWithBounce bx bvx  0 (gw-bw)
    (by',bvy') = if (and [bx >= btx-bw, 
                                bx <= (btx+btw), 
                                by >= (bty-bh), 
                                by <= bty,
                                bvy > 0])
                        then incrementWithBounce by bvy 0 (bty-bh)
                        else incrementWithBounce by bvy 0 (gh-bh)
    ball' = if (by+bvy) >= (gh-bh) 
            then newBall
            else ball{bx=bx',by=by',bvx=bvx',bvy=bvy'}
