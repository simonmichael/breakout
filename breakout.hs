{-# LANGUAGE ScopedTypeVariables #-}
{-
A Breakout prototype, for exploring haskell game dev.
Simon Michael (c) 2007-2020, GPLv3+
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative.Simple
import qualified SDL.Framerate as Framerate
import System.IO (stderr, hPutStrLn)

import Constants
import Game
import Graphics
import Sound
import Data.Tini.Configurable -- hiding (empty)

import qualified Data.Text as T

main :: IO ()
main = do
  -- parse Opts, defined in Game.hs
  (opts'@Opts{..},()) <- simpleOptions
    progversion
    usageheading
    usagebody
    (Opts 
      <$> (switch $ long "debug" <> help "show debug info")
      <*> (optional $ option auto $ long "for" <> metavar "TICKS" <> help "exit after running the main loop for this many ticks")
    )
    empty

  -- confdir <- getXdgDirectory XdgConfig progname
  -- Just ini <- readIniFile $ confdir </> progname <.> "conf" 

  initializeAll
  -- convert Opts{oendtick} from a count of ticks to a future SDL tick time
  tick0 <- ticks
  let opts = opts'{oendtick = fmap (tick0 +) oendtick}

  withSounds $ \sounds -> do
    withFonts $ \fonts -> do
      window <- createWindow (T.pack progname) defaultWindow{ 
        windowInitialSize = V2 defwindowwidth defwindowheight ,
        -- windowMode = FullscreenDesktop,
        windowPosition = Centered
        }
      raiseWindow window
      cursorVisible $= False
      renderer <- createRenderer window (-1) defaultRenderer{ rendererType = AcceleratedVSyncRenderer }
      Framerate.with framerate $ \fpsmgr -> do
        tnow <- time
        let game0 = newGame opts defaultConfig window renderer fpsmgr sounds fonts defwindowwidth defwindowwidth tnow
        (game, merr) <- gameLoad game0
        case merr of
          Just err -> hPutStrLn stderr $ "Could not load high score: " ++ err
          Nothing  -> return ()
        gameLoop game
