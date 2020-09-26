{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Game where
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Data.List (foldl')
import qualified Data.Text as T
import Foreign.C (CInt)
import SDL hiding (trace)
import qualified SDL.Framerate as Framerate
import System.Exit (exitSuccess)

import Ball
import Bat
import Graphics
import Sound
import Constants
import Util
import Control.Concurrent (threadDelay)
import Data.Word (Word32)
import Data.Maybe (fromJust)

type Score = Integer

-- app, window, game state
data Game = Game {
  -- config
  gendtick :: Maybe Word32,  -- a time in SDL ticks after which the program should exit
  -- resources
  gwindow :: Window,
  grenderer :: Renderer,
  gfpsmgr :: Framerate.Manager,
  gsounds :: Sounds,
  gfonts :: Fonts,
  -- game state
  gw :: CInt,         -- window dimensions
  gh :: CInt,
  gmode :: GameMode,  -- current game mode/scene
  gtoplay :: [Sound], -- new sounds to play
  gschedule :: Seconds,  -- if non-zero, future value of SDL.time at which some pending thing should happen
  -- game objects
  gbat :: Bat,
  gball :: Ball,
  gscore :: Score,
  -- current user input
  gQPressed :: Bool,
  gPPressed :: Bool,
  gshiftPPressed :: Bool,
  gleftPressed :: Bool,
  grightPressed :: Bool,
  gspacePressed :: Bool
  }
  deriving Show

data GameMode = GameAttract | GamePlay | GamePause | GamePauseScreenshot | GameOver | GameExit
  deriving (Eq,Show)

-- data GameEvent = Quit | Sound Sound

newGame :: Maybe Word32 -> Window -> Renderer -> Framerate.Manager -> Sounds -> Fonts -> CInt -> CInt -> Game
newGame totick window renderer fpsmgr sounds fonts width height = Game {
  gendtick = totick,
  --
  gwindow = window,
  grenderer = renderer,
  gfpsmgr = fpsmgr,
  gsounds = sounds,
  gfonts = fonts,
  --
  gw = width,
  gh = height,
  gmode = GameAttract,
  gtoplay = [],
  gschedule = 0,
  --
  gbat = newBat width height,
  gball = newBall,
  gscore = 0,
  --
  gQPressed = False,
  gPPressed = False,
  gshiftPPressed = False,
  gleftPressed = False,
  grightPressed = False,
  gspacePressed = False
  }

gameReset :: Game -> Game
gameReset Game{..} = newGame gendtick gwindow grenderer gfpsmgr gsounds gfonts gw gh

gameClearInput :: Game -> Game
gameClearInput g = g{
  gQPressed = False,
  gPPressed = False,
  gshiftPPressed = False,
  gleftPressed = False,
  grightPressed = False,
  gspacePressed = False
  }

gameLoop :: Game -> IO ()
gameLoop game@Game{gendtick,gwindow,grenderer,gfpsmgr,gsounds,gfonts,gw,gh} = do
  tticks <- ticks
  tsecs <- time
  game' <- gameProcessSdlEvents game
  let game'' = gameStep tsecs game'
  if (gmode game'' /= GameExit && (gendtick==Nothing || tticks < fromJust gendtick))
  then do
    gameDraw game''
    present grenderer
    game''' <- gamePlayNewSounds game''
    delay <- Framerate.delay gfpsmgr
    gameLoop game'''
  else do
    -- Try to encourage good behaviour when exiting within GHCI.
    -- Usually the window does get destroyed, but not always.
    -- Often/always a process/app switcher icon remains, until
    -- GHCI exits, and can steal focus, and won't show the mouse
    -- cursor until you switch away from it.
    cursorVisible $= True
    destroyWindow gwindow

gameProcessSdlEvents :: Game -> IO Game
gameProcessSdlEvents game = pollEvents >>= return . foldl' processEvent game 
  where
    processEvent game event
      | event `isKeyDn` KeycodeQ || event `isKeyDn` KeycodeEscape = game {gQPressed = True}

      | event `isKeyDn` KeycodeP && eventHasShift event = game {gshiftPPressed = True}
      | event `isKeyUp` KeycodeP && eventHasShift event = game {gshiftPPressed = False}
      | event `isKeyDn` KeycodeP                        = game {gPPressed = True}
      | event `isKeyUp` KeycodeP                        = game {gPPressed = False}

      | event `isKeyDn` KeycodeLeft = game {gleftPressed = True}
      | event `isKeyUp` KeycodeLeft = game {gleftPressed = False}
      | event `isKeyDn` KeycodeRight = game {grightPressed = True}
      | event `isKeyUp` KeycodeRight = game {grightPressed = False}
      | event `isKeyDn` KeycodeSpace = game {gspacePressed = True}
      | event `isKeyUp` KeycodeSpace = game {gspacePressed = False}

      | otherwise = game

gameStep :: Seconds -> Game -> Game
gameStep tnow game@Game{..} =
  case gmode of

    GameAttract | gQPressed             -> game{gmode=GameExit}
    GameAttract | gspacePressed         -> game{gmode=GamePlay}

    GamePause           | gspacePressed -> game{gmode=GamePlay}
    GamePauseScreenshot | gspacePressed -> game{gmode=GamePlay}
    GamePause           | gQPressed     -> gameReset game
    GamePauseScreenshot | gQPressed     -> gameReset game

    GamePlay    | gshiftPPressed        -> game{gmode=GamePauseScreenshot}
    GamePlay    | gPPressed             -> game{gmode=GamePause}
    GamePlay    | gameover || gQPressed -> game{gmode=GameOver, gQPressed=False, gtoplay=[sndhit gsounds], gschedule=tnow+gameoverdelay}
    GamePlay                            -> game{gbat = bat, gball = ball, gtoplay = batsounds ++ ballsounds, gscore = score}

    GameOver    | gQPressed || gspacePressed || tnow >= gschedule  -> gameReset game
    GameOver                            -> game{gbat = bat, gtoplay = batsounds}

    otherwise                           -> game

  where
    (bat, batsounds) = gameStepBat game gbat
    (ball, ballsounds, score, gameover) = gameStepBall game gball

gameStepBat :: Game -> Bat -> (Bat, [Sound])
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
    sounds = [sndkickdrum | hitwall]

gameStepBall :: Game -> Ball -> (Ball, [Sound], Integer, Bool)
gameStepBall
  game@Game {gsounds = Sounds {..}, gw, gh, gleftPressed, grightPressed, gbat = Bat {..}, gscore}
  ball@Ball {..} =
    (ball', sounds, gscore', isnewball)
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
      (ball', isnewball)
        | (by + bvy) >= (gh - bh) = (newBall, True)
        | otherwise = (ball {bx = bx', by = by', bvx = bvx', bvy = bvy'}, False)
      sounds =
        [sndwall | hitwallx || hitwally]
        ++ [sndpaddle | hitbat]
      gscore'
        | hitbat = gscore + 1
        | otherwise = gscore

gamePlayNewSounds :: Game -> IO Game
gamePlayNewSounds game@Game {gtoplay} = do
  mapM_ play' gtoplay
  return game {gtoplay = []}

gameDraw :: Game -> IO ()
gameDraw game@Game {..} =
  case gfonts of
    Fonts{..} -> do
      t <- fromIntegral <$> ticks
      let c = (black : replicate 40 red) `pickWith` (t `div` 100)
      rendererDrawColor grenderer $= black
      clear grenderer
      let mid = V2 (gw `div` 2) (gh `div` 2)
      case gmode of
        GameAttract -> do
          drawTextCenteredAt grenderer fnt3 1 (mid - V2 0 40) white $ T.toUpper progname
          drawTextCenteredAt grenderer fnt2 1 (mid + V2 0 60) c "SPACE to play"
        GamePlay    -> do
          batDraw grenderer gbat
          ballDraw grenderer gball
          scoreDraw game
        GamePause   -> do
          batDraw grenderer gbat
          ballDraw grenderer gball
          scoreDraw game
          drawTextCenteredAt grenderer fnt2 1 mid c "SPACE to resume"
        GamePauseScreenshot -> do
          batDraw grenderer gbat
          ballDraw grenderer gball
          scoreDraw game
        GameOver    -> do
          batDraw grenderer gbat
          scoreDraw game
          drawTextCenteredAt grenderer fnt2 1 mid red "GAME OVER"

scoreDraw :: Game -> IO ()
scoreDraw Game {grenderer, gfonts = Fonts {..}, gw, gh, gscore} = do
  let 
    t = T.pack $ show gscore
    font = fnt2
    scale = 1
  (tw, th) <- both (scale*) <$> textSize font t
  let (tx, ty) = (gw - 10 - tw, gh - 10 - th)
      rect = Rectangle (P $ V2 tx ty) (V2 tw th)
  drawText grenderer font white rect t

