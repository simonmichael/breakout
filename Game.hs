{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game where
import Control.Exception
import Control.Monad
import Data.List (foldl')
import qualified Data.Text as T
import Safe
import SDL hiding (trace)
import qualified SDL.Framerate as Framerate
import System.Directory
import System.FilePath

import Ball
import Bat
import Brick
import Graphics
import Sound
import Constants
import Util
import Data.Maybe (fromJust)
import Data.Tini.Configurable
import System.IO (stderr, hPutStrLn)
import Text.Printf (printf)

-- Command line options.
data Opts = Opts
  { odebug :: Bool
  , oendtick :: Maybe Tick  -- an SDL ticks value at which the program should exit
  }
  deriving Show

type Score = Int

-- Persistent state saved to filesystem.
data SavedState = SavedState
  { sshighscore :: Score
  } deriving (Generic, Show, Read, Eq)

instance Configurable SavedState where
  defaultConfig = SavedState { sshighscore = 0 }

-- App, window, game state.
data Game = Game {
  -- config
  gopts :: Opts,
  gsaved :: SavedState,
  -- resources
  gwindow :: Window,
  grenderer :: Renderer,
  gfpsmgr :: Framerate.Manager,
  gsounds :: Sounds,
  gfonts :: Fonts,
  -- current user input
  gQPressed :: Bool,
  gPPressed :: Bool,
  gshiftPPressed :: Bool,
  gleftPressed :: Bool,
  grightPressed :: Bool,
  gspacePressed :: Bool,
  -- game state
  gw :: CInt,         -- window dimensions
  gh :: CInt,
  gstartTime :: SDLTime,  -- value of SDL.time at which the program started running
  glastStepTime :: SDLTime, -- value of SDL.time at which the game loop last stepped
  gframe :: FrameNum,  -- what drawing frame / game step number is this (how many times have we called Framerate.delay)
  gfps :: Double,  -- approximate simple moving average of frames drawn per second, updated each step
  gfpsdisplay :: Double,  -- a copy of gfps updated less often, for display. Clunky!
  gnewsounds :: [Sound], -- new sounds to play
  gmode :: GameMode,  -- current game mode/scene
  gmodeStartTime :: SDLTime,  -- value of SDL.time at which the current (non-Pause) mode was entered
  gmodeAge :: Seconds,  -- how long have we been in this mode (excluding Pause mode)
  gscore :: Score,
  -- game objects
  gbat :: Bat,
  gball :: Ball,
  gbricks :: [Brick]
  }
  deriving Show

data GameMode = GameAttract | GamePlay | GamePause | GamePauseScreenshot | GameOver | GameOverHighScore | GameExit
  deriving (Eq,Show)

newGame :: Opts -> SavedState -> Window -> Renderer -> Framerate.Manager -> Sounds -> Fonts -> CInt -> CInt -> SDLTime -> Game
newGame opts saved window renderer fpsmgr sounds fonts width height starttime = Game {
  gopts = opts,
  gsaved = saved,
  --
  gwindow = window,
  grenderer = renderer,
  gfpsmgr = fpsmgr,
  gsounds = sounds,
  gfonts = fonts,
  --
  gQPressed = False,
  gPPressed = False,
  gshiftPPressed = False,
  gleftPressed = False,
  grightPressed = False,
  gspacePressed = False,
  --
  gw = width,
  gh = height,
  gstartTime = starttime,
  glastStepTime = starttime,
  gframe = 0,
  gfps = 0,
  gfpsdisplay = 0,
  gnewsounds = [],
  gmode = GameAttract,
  gmodeStartTime = starttime,
  gmodeAge = 0,
  gscore = 0,
  --
  gbat = newBat width height,
  gball = newBall,
  gbricks = newBricks 5
  }

-- Reset most game state, except for: 
-- the saved state, program start time, game last step time, frame count, recent average fps;
-- and set the mode's start time.
gameReset :: SDLTime -> Game -> Game
gameReset tnow Game{..} = 
  (newGame gopts gsaved gwindow grenderer gfpsmgr gsounds gfonts gw gh 0)
  {gstartTime=gstartTime, glastStepTime=glastStepTime, gframe=gframe, gfps=gfps, gsaved=gsaved, gmodeStartTime=tnow}

-- Switch the game mode, setting the new mode's start time and age.
gameModeSwitch :: SDLTime -> GameMode -> Game -> Game
gameModeSwitch tnow mode game = game{gmode=mode, gmodeStartTime=tnow, gmodeAge=0}

-- Give the current SDL time, update the game mode's current age.
gameModeAgeUpdate :: SDLTime -> Game -> Game
gameModeAgeUpdate tnow game@Game{..} = game{gmodeAge = tnow - gmodeStartTime} 

-- Add the given sound, named by a Sounds field accessor, to the list of sounds to be played.
gameQueueSound :: (Sounds -> Sound) -> Game -> Game
gameQueueSound snd game@Game{..} = game{gnewsounds=gnewsounds++[snd gsounds]}

-- Add the given Sounds to the list of sounds to be played.
gameQueueSounds :: [Sound] -> Game -> Game
gameQueueSounds snds game@Game{..} = game{gnewsounds=gnewsounds++snds}

-- Reset keypress indicators.
gameClearInput :: Game -> Game
gameClearInput g = g{
  gQPressed = False,
  gPPressed = False,
  gshiftPPressed = False,
  gleftPressed = False,
  grightPressed = False,
  gspacePressed = False
  }

-- Where to save persistent state.
saveFile :: IO FilePath
saveFile = (</> progname <.> "save") <$> getXdgDirectory XdgData progname

-- Update the game's saveable state from the filesystem.
-- If that fails, return the game unchanged and an error message.
gameLoad :: Game -> IO (Game, Maybe String)
gameLoad game = do
  -- save <- saveFile >>= readConfigFile
  handle (\(e::IOException) -> return (game, Just $ show e)) $ do
    s <- saveFile >>= readFile
    return $
      case readEitherSafe s of
        Right save -> (game{gsaved=save}, Nothing)
        Left e     -> (game, Just e)

-- Write the game's saveable state to the filesystem, if possible.
-- Catch any error and return the message.
gameSave :: Game -> IO (Maybe String)
gameSave Game{gsaved} = handle (\(e::IOException) -> return $ Just $ show e) $ do
  f <- saveFile
  createDirectoryIfMissing True $ takeDirectory f
  -- writeConfigFile f gsaved  -- https://github.com/valderman/tini/issues/1, fixed in tini 0.1.0.1
  writeFile f $ show gsaved
  return Nothing    

gameLoop :: Game -> IO ()
gameLoop game@Game{gopts=Opts{oendtick},gwindow,grenderer,gfpsmgr,gframe} = do
  tticks <- ticks
  tnow <- time
  -- frame <- Framerate.count gfpsmgr  -- resets to zero now and then, reason unknown
  -- when (frame < gframe) $ error $ "gframe: " ++ show gframe ++ " frame: " ++ show frame
  let frame = gframe+1
  game' <- gameProcessSdlEvents game
  let game'' = gameStep tnow frame game'

  -- save high score if it has changed
  when (sshighscore (gsaved game'') /= sshighscore (gsaved game)) $ do
    merr <- gameSave game''
    case merr of
      Just err -> hPutStrLn stderr $ "Could not save high score: " ++ err
      Nothing  -> return ()

  if (gmode game'' /= GameExit && (oendtick==Nothing || tticks < fromJust oendtick))
  then do
    gameDraw game''
    present grenderer
    game''' <- gamePlayNewSounds game''
    _elapsed <- Framerate.delay gfpsmgr
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

gameStep :: Seconds -> FrameNum -> Game -> Game
gameStep tnow frame game'@Game{..} =
  let 
    game = gameUpdateStats game' tnow frame
    (bat, batsounds) = gameStepBat game gbat
    (ball, ballsounds, score, gameover) = gameStepBall game gball
    gameQuit = gameQueueSound sndballLoss $ gameReset tnow game
    dbg 
      --  | odebug gopts = \msg -> trace (show gmode++" "++msg)
      | otherwise = const id
    SavedState{sshighscore} = gsaved

  in case gmode of

    GameAttract | gQPressed             -> dbg "q" $ gameModeSwitch tnow GameExit game
    GameAttract | gspacePressed         -> dbg "space" $ gameModeSwitch tnow GamePlay game

    GamePause           | gspacePressed -> dbg "space" $ gameModeSwitch tnow GamePlay game
    GamePauseScreenshot | gspacePressed -> dbg "space" $ gameModeSwitch tnow GamePlay game
    GamePause           | gQPressed     -> dbg "q" $ gameQuit
    GamePauseScreenshot | gQPressed     -> dbg "q" $ gameQuit

    GamePlay    | gshiftPPressed        -> dbg "P" $ gameModeSwitch tnow GamePauseScreenshot game
    GamePlay    | gPPressed             -> dbg "p" $ gameModeSwitch tnow GamePause game
    GamePlay    | gQPressed             -> dbg "q" $ gameQuit
    GamePlay    | gameover && gscore > sshighscore 
                                        -> dbg "game over high score" $ gameModeSwitch tnow GameOverHighScore $ 
                                            gameQueueSound sndballLoss $ game{gsaved=gsaved{sshighscore=gscore}}
    GamePlay    | gameover              -> dbg "game over" $ gameModeSwitch tnow GameOver $ gameQueueSound sndballLoss game
    GamePlay                            -> dbg "" $ gameQueueSounds (batsounds++ballsounds) game{gbat = bat, gball = ball, gscore = score}

    m | m `elem` [GameOver, GameOverHighScore] && (gQPressed || gspacePressed || gmodeAge >= gameoverdelay)
                                        -> dbg "q/space/timeout" $ gameReset tnow game
    m | m `elem` [GameOver, GameOverHighScore]                            
                                        -> dbg "" $ gameQueueSounds batsounds game{gbat = bat}

    _                                   -> dbg "" $ game

-- Update game's last step time, FPS, age of current game mode, etc.
gameUpdateStats :: Game -> Seconds -> FrameNum -> Game
gameUpdateStats game@Game{..} tnow frame =
  let
    fpssamples = 10
    fpsdisplayupdatesps = 4
    lastframetime = tnow - glastStepTime
    lastfps = 1 / lastframetime
    gfps' = gfps * (fpssamples-1)/fpssamples + lastfps/fpssamples
    gfpsdisplay' =
      if frame `mod` (framerate `div` fpsdisplayupdatesps) == 0
      then gfps' else gfpsdisplay
  in
    gameModeAgeUpdate tnow game{glastStepTime=tnow, gframe=frame, gfps=gfps', gfpsdisplay=gfpsdisplay'}

gameStepBat :: Game -> Bat -> (Bat, [Sound])
gameStepBat Game {gsounds = Sounds {..}, gw, gh, gleftPressed, grightPressed} bat@Bat {..} = (bat', sounds)
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
    sounds = [sndbatWall | hitwall]

gameStepBall :: Game -> Ball -> (Ball, [Sound], Score, Bool)
gameStepBall
  Game {gsounds = Sounds {..}, gw, gh, gbat = Bat {..}, gscore}
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
           [sndballWall | hitwallx || hitwally]
        ++ [sndballBat  | hitbat]
      gscore'
        | hitbat = gscore + 1
        | otherwise = gscore

gamePlayNewSounds :: Game -> IO Game
gamePlayNewSounds game@Game {gnewsounds} = do
  mapM_ play' gnewsounds
  return game {gnewsounds = []}

gameDraw :: Game -> IO ()
gameDraw game@Game {..} =
  case gfonts of
    Fonts{..} -> do
      t <- fromIntegral <$> ticks
      let 
        unstablered = (black : replicate 40 red) `pickWith` (t `div` 100)
        mid = V2 (gw `div` 2) (gh `div` 2)

      clearWith grenderer black
      case gmode of

        GameAttract -> do
          bricksDraw grenderer $ newBricks 19
          let highscore = "High score: " <> T.pack (show $ sshighscore gsaved)
          let msgs = [
                "",
                "SPACE to play",
                "LEFT/RIGHT to move",
                "P to pause",
                "Q/ESC to quit"
                ]
              msg = msgs `pickWith` (round gmodeAge `div` 2)
          drawTextCenteredAt grenderer fnt3 1 (mid - V2 0 100) white $ T.toUpper $ T.pack progname
          drawTextCenteredAt grenderer fnt2 1 (mid + V2 0 0) red msg
          drawTextCenteredAt grenderer fnt2 1 (mid + V2 0 100) white highscore -- & when (gmodeAge > 1)
            -- something seems off with gmodeAge at start, highscore at t=1 appears too close before msg at t=2
          debugInfoDraw game

        GamePlay -> do
          bricksDraw grenderer gbricks
          batDraw grenderer gbat
          ballDraw grenderer gball
          scoreDraw game
          debugInfoDraw game

        m | m `elem` [GamePause, GamePauseScreenshot] -> do
          bricksDraw grenderer gbricks
          batDraw grenderer gbat
          ballDraw grenderer gball
          scoreDraw game
          debugInfoDraw game
          unless (m==GamePauseScreenshot) $
            drawTextCenteredAt grenderer fnt2 1 mid unstablered "SPACE to resume"
  
        m | m `elem` [GameOver, GameOverHighScore] -> do
          bricksDraw grenderer gbricks
          batDraw grenderer gbat
          scoreDraw game
          debugInfoDraw game
          drawTextCenteredAt grenderer fnt2 1 mid red "GAME OVER"
          when (m==GameOverHighScore) $
            drawTextCenteredAt grenderer fnt2 1 (mid + V2 0 100) white $ "New high score !"

scoreDraw :: Game -> IO ()
scoreDraw Game {grenderer, gfonts = Fonts {..}, gw, gh, gscore} = do
  let 
    t = T.pack $ show gscore
    font = fnt2
    scale = 1
  (tw, th) <- both (scale*) <$> textSize font t
  let (tx, ty) = (gw - 5 - tw, gh - 5 - th)
      rect = Rectangle (P $ V2 tx ty) (V2 tw th)
  drawText grenderer font white rect t

debugInfoDraw :: Game -> IO ()
debugInfoDraw Game {..} = do
  let 
    font = fnt1 gfonts
    scale = 1
    t | gfpsdisplay == 0 = ""
      | otherwise        = T.pack $ printf "FPS: %0.f" gfpsdisplay
      -- "FPS: %0.1f frame: %d" gfps gframe
  (tw, th) <- both (scale*) <$> textSize font t
  let (tx, ty) = (5, gh - 5 - th)
      rect = Rectangle (P $ V2 tx ty) (V2 tw th)
  drawText grenderer font white rect t
