{-
A Breakout prototype, for exploring haskell game dev.
Simon Michael (c) 2007-2020, GPLv3+
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.List (partition)
import Data.Text (Text)
import SDL hiding (trace)
import SDL.Mixer
import SDL.Primitive
import qualified SDL.Framerate as Framerate
import System.Exit (exitSuccess)

import Util
import Types
import Game
import Draw
import Sound

progname = "breakout"

-- Our aspirational frame rate. We will show no more than this many frames per second,
-- enforced by Framerate.delay below. 
-- On mac at least, vsync (rendererType = AcceleratedVSyncRenderer below) also enforces
-- a max frame rate of 60 fps. 
framerate = 60  -- fps

defscreenwidth = 400
defscreenheight = 400

main :: IO ()
main = do
  initializeAll
  withSounds $ \sounds@Sounds{..} -> do
    window <- createWindow progname defaultWindow{ 
      windowInitialSize = V2 defscreenwidth defscreenheight ,
      windowPosition = Centered
      }
    renderer <- createRenderer window (-1) defaultRenderer{ rendererType = AcceleratedVSyncRenderer }
    Framerate.with framerate $ \fpsmgr -> do
      loop (newGame window renderer fpsmgr sounds defscreenwidth defscreenheight)

loop :: Game -> IO ()
loop game@Game{..} = do
  events <- pollEvents
  when (any (flip isKeyDn KeycodeQ) events) $ do
    destroyWindow gwindow   -- get rid of window when in GHCI.. not working
    exitSuccess

  game' <- foldM gameHandleEvent game events
  game''@Game{gfpsmgr} <- gamePlayNewSounds $ gameStep game'
  gameDraw game''
  delay <- Framerate.delay gfpsmgr
  -- mtrace "delay" delay
  loop game''
