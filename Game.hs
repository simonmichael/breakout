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

-- app & game state
data Game = Game {
      window :: Window,
      renderer :: Renderer,
      fpsmgr :: Manager,
      screenw :: CInt,
      screenh :: CInt,
      leftPressed :: Bool,
      rightPressed :: Bool,
      bat :: Bat,
      ball :: Ball
}

newGame :: Window -> Renderer -> Manager -> CInt -> CInt -> Game
newGame window renderer fpsmgr screenw screenh = Game {
  window = window,
  renderer = renderer,
  fpsmgr = fpsmgr,
  screenw = screenw,
  screenh = screenh,
  leftPressed = False,
  rightPressed = False,
  bat = newBat (div screenw 2) (screenh-defbatheight-40),
  ball = newBall
}

gameHandleEvent :: Game -> Event -> IO Game
gameHandleEvent game event =
  -- mtrace "evdata" evdata
  -- mtrace "keycode" $ keysymKeycode keyboardEventKeysym
  case event of
    _
      --  | event `isKeyDn` KeycodeQ -> gameExit game >> return game
      | event `isKeyDn` KeycodeLeft -> return game{leftPressed=True}
      | event `isKeyUp` KeycodeLeft -> return game{leftPressed=False}
      | event `isKeyDn` KeycodeRight -> return game{rightPressed=True}
      | event `isKeyUp` KeycodeRight -> return game{rightPressed=False}
      | otherwise -> return game

gameStep :: Game -> Game
gameStep game@Game{screenw, screenh, leftPressed, rightPressed, bat=bat@Bat{..}, ball=ball@Ball{..}} =
  game{bat=bat', ball=ball'}
  where
    bat' = gameStepBat game bat
    ball' = gameStepBall game ball

gameStepBat :: Game -> Bat -> Bat
gameStepBat game@Game{screenw, screenh, leftPressed, rightPressed} bat@Bat{..} = bat'
  where
    batvx' = if leftPressed then (max (batvx-bataccel) (-batmaxspeed)) else batvx
    batvx'' = if rightPressed then (min (batvx'+bataccel) (batmaxspeed)) else batvx'
    batvx''' = if (and [not leftPressed, not rightPressed]) then truncate(fromIntegral batvx'' * (1.0-defbatfriction)) else batvx''
    (batx',batvx'''') = incrementWithStop batx  batvx''' 0 (screenw-batw)
    (baty',batvy') = (screenh-bath-40, 0)
    bat' = bat{batx=batx',baty=baty',batvx=batvx''',batvy=batvy'}

gameStepBall :: Game -> Ball -> Ball
gameStepBall game@Game{screenw, screenh, leftPressed, rightPressed, bat=Bat{..}} ball@Ball{..} = ball'
  where
    (ballx',ballvx') = incrementWithBounce ballx ballvx  0 (screenw-ballw)
    (bally',ballvy') = if (and [ballx >= batx-ballw, 
                                ballx <= (batx+batw), 
                                bally >= (baty-ballh), 
                                bally <= baty,
                                ballvy > 0])
                        then incrementWithBounce bally ballvy 0 (baty-ballh)
                        else incrementWithBounce bally ballvy 0 (screenh-ballh)
    ball' = if (bally+ballvy) >= (screenh-ballh) 
            then newBall
            else ball{ballx=ballx',bally=bally',ballvx=ballvx',ballvy=ballvy'}
