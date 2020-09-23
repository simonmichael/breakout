{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Game where

import Ball
import Bat
import qualified Data.Text as T
import Draw
import Foreign.C (CInt)
import SDL hiding (trace)
import SDL.Framerate (Manager)
import Sound
import Types
import Util
import Control.Monad.IO.Class (MonadIO)

type Score = Integer

-- app, window, game state
data Game = Game
  { gwindow :: Window,
    grenderer :: Renderer,
    gfpsmgr :: Manager,
    gsounds :: Sounds,
    gfonts :: Fonts,
    gtoplay :: [Chunk],
    gw :: CInt,
    gh :: CInt,
    gleftPressed :: Bool,
    grightPressed :: Bool,
    gbat :: Bat,
    gball :: Ball,
    gscore :: Score
  }

newGame :: Window -> Renderer -> Manager -> Sounds -> Fonts -> CInt -> CInt -> Game
newGame window renderer fpsmgr sounds fonts width height =
  Game
    { gwindow = window,
      grenderer = renderer,
      gfpsmgr = fpsmgr,
      gsounds = sounds,
      gfonts = fonts,
      gtoplay = [],
      gw = width,
      gh = height,
      gleftPressed = False,
      grightPressed = False,
      gbat = newBat (div width 2) (height - defbatheight -40),
      gball = newBall,
      gscore = 0
    }

gameHandleEvent :: Game -> Event -> IO Game
gameHandleEvent game event =
  -- mtrace "evdata" evdata
  -- mtrace "keycode" $ keysymKeycode keyboardEventKeysym
  case event of
    _
      --  | event `isKeyDn` KeycodeQ -> gameExit game >> return game
      | event `isKeyDn` KeycodeLeft -> return game {gleftPressed = True}
      | event `isKeyUp` KeycodeLeft -> return game {gleftPressed = False}
      | event `isKeyDn` KeycodeRight -> return game {grightPressed = True}
      | event `isKeyUp` KeycodeRight -> return game {grightPressed = False}
      | otherwise -> return game

gameStep :: Game -> Game
gameStep game@Game {gtoplay, gw, gh, gleftPressed, grightPressed, gbat = bat@Bat {..}, gball = ball@Ball {..}} =
  game {gbat = bat', gball = ball', gtoplay = gtoplay ++ batsounds ++ ballsounds, gscore = score'}
  where
    (bat', batsounds) = gameStepBat game bat
    (ball', ballsounds, score') = gameStepBall game ball

gameStepBat :: Game -> Bat -> (Bat, [Chunk])
gameStepBat game@Game {gsounds = Sounds {..}, gw, gh, gleftPressed, grightPressed} bat@Bat {..} = (bat', sounds)
  where
    btvx' = if gleftPressed then (max (btvx - btaccel) (- btmaxspeed)) else btvx
    btvx'' = if grightPressed then (min (btvx' + btaccel) (btmaxspeed)) else btvx'
    btvx''' =
      if (and [not gleftPressed, not grightPressed])
        then truncate (fromIntegral btvx'' * (1.0 - defbatfriction)) -- friction has little effect because of truncate
        else btvx''
    (btx', btvx'''', hitwall) = incrementWithStop btx btvx''' 0 (gw - btw)
    (bty', btvy') = (gh - bth -40, 0)
    bat' = bat {btx = btx', bty = bty', btvx = btvx'''', btvy = btvy'}
    sounds = [sndkickdrum1 | hitwall]

gameStepBall :: Game -> Ball -> (Ball, [Chunk], Integer)
gameStepBall
  game@Game {gsounds = Sounds {..}, gw, gh, gleftPressed, grightPressed, gbat = Bat {..}, gscore}
  ball@Ball {..} =
    (ball', sounds, gscore')
    where
      (bx', bvx', hitwallx) = incrementWithBounce bx bvx 0 (gw - bw)
      (by', bvy', hitwally, hitbat)
        | and
            [ bx >= btx - bw,
              bx <= (btx + btw),
              by >= (bty - bh),
              by <= bty,
              bvy > 0
            ] =
          let (x, y, _) = incrementWithBounce by bvy 0 (bty - bh) in (x, y, False, True)
        | otherwise =
          let (x, y, hitwally) = incrementWithBounce by bvy 0 (gh - bh) in (x, y, hitwally, False)
      (ball', newball)
        | (by + bvy) >= (gh - bh) = (newBall, True)
        | otherwise = (ball {bx = bx', by = by', bvx = bvx', bvy = bvy'}, False)
      sounds =
        [sndwall | hitwallx || hitwally]
          ++ [sndpaddle | hitbat]
          ++ [sndbassdrum1 | newball]
      gscore'
        | newball = 0
        | hitbat = gscore + 1
        | otherwise = gscore + 0  -- 1

gamePlayNewSounds :: Game -> IO Game
gamePlayNewSounds game@Game {gtoplay} = do
  mapM_ play' gtoplay
  return game {gtoplay = []}

gameDraw :: Game -> IO ()
gameDraw game@Game {..} = do
  rendererDrawColor grenderer $= black
  clear grenderer
  batDraw grenderer gbat
  ballDraw grenderer gball
  scoreDraw game

scoreDraw :: Game -> IO ()
scoreDraw game@Game{grenderer, gfonts=Fonts {..}, gw, gh, gscore} = do
  let t = T.pack $ show gscore
  (tw',th') <- size fntcrystal t
  let 
    (tw,th) = (2 * fromIntegral tw', 2 * fromIntegral th')  -- LOSSY
    (tx,ty) = (gw-10-tw, gh-10-th)
    rect = Rectangle (P $ V2 tx ty) (V2 tw th)
  drawText game white rect t

drawText :: MonadIO m => Game -> Color -> Rectangle CInt -> T.Text -> m ()
drawText Game {grenderer, gfonts = Fonts{..}} color rect t = do
  s <- blended fntcrystal color t
  t <- createTextureFromSurface grenderer s
  copy grenderer t Nothing (Just rect)

