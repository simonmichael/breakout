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

import qualified SDL.Framerate as Framerate
import System.Exit (exitSuccess)

import Util
import Constants
import Game
import Draw
import Sound

main :: IO ()
main = do
  initializeAll
  withSounds $ \sounds@Sounds{..} -> 
    withFonts $ \fonts -> do
      window <- createWindow progname defaultWindow{ 
        windowInitialSize = V2 defscreenwidth defscreenheight ,
        -- windowMode = FullscreenDesktop,
        windowPosition = Centered
        }
      raiseWindow window
      renderer <- createRenderer window (-1) defaultRenderer{ rendererType = AcceleratedVSyncRenderer }
      Framerate.with framerate $ \fpsmgr -> do
        gameLoop (newGame window renderer fpsmgr sounds fonts defscreenwidth defscreenheight)

