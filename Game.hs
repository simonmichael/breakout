{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Game
where
import Foreign.C (CInt)
import SDL hiding (trace)
import SDL.Framerate (Manager)

import Util
import Types
import Sound
import Bat
import Ball

-- app, window, game state
data Game = Game {
  gwindow :: Window,
  grenderer :: Renderer,
  gfpsmgr :: Manager,
  gsounds :: Sounds,
  gtoplay :: [Chunk],
  gw :: CInt,
  gh :: CInt,
  gleftPressed :: Bool,
  grightPressed :: Bool,
  gbat :: Bat,
  gball :: Ball
}

newGame :: Window -> Renderer -> Manager -> Sounds -> CInt -> CInt -> Game
newGame window renderer fpsmgr sounds width height = Game {
  gwindow = window,
  grenderer = renderer,
  gfpsmgr = fpsmgr,
  gsounds = sounds,
  gtoplay = [],
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
gameStep game@Game{gtoplay, gw, gh, gleftPressed, grightPressed, gbat=bat@Bat{..}, gball=ball@Ball{..}} =
  game{gbat=bat', gball=ball', gtoplay=gtoplay++batsounds++ballsounds}
  where
    (bat',batsounds) = gameStepBat game bat
    (ball',ballsounds) = gameStepBall game ball

gameStepBat :: Game -> Bat -> (Bat, [Chunk])
gameStepBat game@Game{gsounds=Sounds{..}, gw, gh, gleftPressed, grightPressed} bat@Bat{..} = (bat',sounds)
  where
    btvx' = if gleftPressed then (max (btvx-btaccel) (-btmaxspeed)) else btvx
    btvx'' = if grightPressed then (min (btvx'+btaccel) (btmaxspeed)) else btvx'
    btvx''' = if (and [not gleftPressed, not grightPressed]) 
              then truncate(fromIntegral btvx'' * (1.0-defbatfriction))  -- friction has little effect because of truncate
              else btvx''
    (btx',btvx'''',hitwall) = incrementWithStop btx  btvx''' 0 (gw-btw)
    (bty',btvy') = (gh-bth-40, 0)
    bat' = bat{btx=btx',bty=bty',btvx=btvx'''',btvy=btvy'}
    sounds = [sndkickdrum1 | hitwall]

gameStepBall :: Game -> Ball -> (Ball, [Chunk])
gameStepBall game@Game{gsounds=Sounds{..}, gw, gh, gleftPressed, grightPressed, gbat=Bat{..}} ball@Ball{..} = (ball',sounds)
  where
    (bx',bvx',hitwallx) = incrementWithBounce bx bvx  0 (gw-bw)
    (by',bvy',hitwally,hitbat)
      | and [bx >= btx-bw, 
              bx <= (btx+btw), 
              by >= (bty-bh), 
              by <= bty,
              bvy > 0] = 
        let (x,y,_) = incrementWithBounce by bvy 0 (bty-bh) in (x,y,False,True)
      | otherwise = 
        let (x,y,hitwally) = incrementWithBounce by bvy 0 (gh-bh) in (x,y,hitwally,False)
    (ball', newball) 
      | (by+bvy) >= (gh-bh) = (newBall, True)
      | otherwise = (ball{bx=bx',by=by',bvx=bvx',bvy=bvy'}, False)
    sounds =
      [sndwall | hitwallx || hitwally] ++
      [sndpaddle | hitbat] ++
      [sndbassdrum1 | newball]

gamePlayNewSounds :: Game -> IO Game
gamePlayNewSounds game@Game{gtoplay} = do
  mapM_ play' gtoplay
  return game{gtoplay=[]}

