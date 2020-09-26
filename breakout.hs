{-# LANGUAGE ScopedTypeVariables #-}
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
import Options.Applicative.Simple
import qualified SDL.Framerate as Framerate
import System.Environment
import System.Exit (exitSuccess)

import Util
import Constants
import Game
import Graphics
import Sound
import Data.Word (Word32)
import Data.Time.Clock

import Options.Applicative.Simple

main :: IO ()
main = do
  (for :: Maybe Word32,()) <- simpleOptions 
    progversion
    usageheading
    usagebody
    (optional $ option auto $ long "for" <> metavar "TICKS"
      <> help "exit after running the main loop for this many ticks")
    empty

  initializeAll
  withSounds $ \sounds@Sounds{..} -> do
    withFonts $ \fonts -> do
      window <- createWindow progname defaultWindow{ 
        windowInitialSize = V2 defwindowwidth defwindowheight ,
        -- windowMode = FullscreenDesktop,
        windowPosition = Centered
        }
      raiseWindow window
      renderer <- createRenderer window (-1) defaultRenderer{ rendererType = AcceleratedVSyncRenderer }
      Framerate.with framerate $ \fpsmgr -> do
        initialticks <- ticks
        let endtick = fmap (initialticks +) for
        gameLoop (newGame endtick window renderer fpsmgr sounds fonts defwindowwidth defwindowwidth)

