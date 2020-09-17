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
import Foreign.C.Types (CInt)
-- import Linear (V4(..))
import SDL hiding (trace)
import SDL.Primitive
-- import SDL hiding (initialize, trace)
-- import qualified SDL
-- import qualified SDL.Event as Event
-- import qualified SDL.Image as Image
-- import qualified SDL.Mixer as Mixer
import qualified SDL.Framerate as Framerate
import System.Exit (exitSuccess)

-- debugging
import Debug.Trace
-- | trace (print on stdout at runtime) a showable expression
-- (for easily tracing in the middle of a complex expression)
strace :: Show a => a -> a
strace a = trace (show a) a
-- | labelled trace - like strace, with a label prepended
ltrace :: Show a => String -> a -> a
ltrace l a = trace (l ++ ": " ++ show a) a
-- | monadic trace - like ltrace, but works as a standalone line in a monad
mtrace :: (Monad m, Show a) => String -> a -> m a
mtrace l a = ltrace l a `seq` return a
-- | trace an expression using a custom show function
tracewith :: (a -> String) -> a -> a
tracewith f e = trace (f e) e



progname = "breakout"

-- Our aspirational frame rate. We will show no more than this many frames per second,
-- enforced by Framerate.delay below. 
-- On mac at least, vsync (rendererType = AcceleratedVSyncRenderer below) also enforces
-- a max frame rate of 60 fps. 
framerate   = 60  -- fps

screenwidth  = 400
screenheight = 400
batfriction = 0.2
defbataccel = 2

data Game = Game {
      window :: Window,
      renderer :: Renderer,
      fpsmgr :: Framerate.Manager,
      running :: Bool,
      screenw :: CInt,
      screenh :: CInt,
      leftDown :: Bool,
      rightDown :: Bool,
      bat :: Bat,
      ball :: Ball
}

data Bat = Bat {
      batx :: CInt,
      baty :: CInt,
      batvx :: CInt,
      batvy :: CInt,
      batmaxspeed :: CInt,
      bataccel :: CInt,
      batw :: CInt,
      bath :: CInt
}

data Ball = Ball {
      ballx :: CInt,
      bally :: CInt,
      ballvx :: CInt,
      ballvy :: CInt,
      ballmaxspeed :: CInt,
      ballw :: CInt,
      ballh :: CInt
}

newGame :: Window -> Renderer -> Framerate.Manager -> Game
newGame window renderer fpsmgr = Game window renderer fpsmgr True screenwidth screenheight False False newBat newBall

newBat :: Bat
newBat  = Bat (div screenwidth 2) (screenheight-h-40) 0 0 10 defbataccel w h where w = 60; h = 10

newBall :: Ball
newBall = Ball 0 0 1 1 0 8 8

main = do
  initializeAll
  window <- createWindow progname defaultWindow{ 
        windowInitialSize = V2 screenwidth screenheight ,
        windowPosition = Centered
        }
  renderer <- createRenderer window (-1) defaultRenderer{ rendererType = AcceleratedVSyncRenderer }
  Framerate.with framerate $ \fpsmgr -> do
      gameLoop (newGame window renderer fpsmgr)

gameLoop :: Game -> IO ()
gameLoop game@Game{..} = do
  (quitevents, events') <- partition eventIsQuit <$> pollEvents
  when (not $ null quitevents) $ do
      -- get rid of window when in GHCI.. not working
      (destroyWindow window :: IO ())  `seq` putStrLn "done."
      exitSuccess

  game' <- foldM gameHandleEvent game events'
  let game''@Game{fpsmgr} = gameStep game'

  delay <- Framerate.delay fpsmgr
--   mtrace "delay" delay

  gameDraw game''
  gameLoop game''

  where
      eventIsQuit event =
            case eventPayload event of
                  KeyboardEvent keyboardEvent ->
                        keyboardEventKeyMotion keyboardEvent == Pressed &&
                        keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
                  _ -> False

gameHandleEvent :: Game -> Event -> IO Game
gameHandleEvent game event =
      case event of
            Event{eventPayload=KeyboardEvent evdata@KeyboardEventData{keyboardEventKeyMotion, keyboardEventKeysym}} -> do
                  -- mtrace "evdata" evdata
                  -- mtrace "keycode" $ keysymKeycode keyboardEventKeysym
                  case keysymKeycode keyboardEventKeysym of
                        KeycodeLeft  -> return game{leftDown=keyboardEventKeyMotion==Pressed}
                        KeycodeRight -> return game{rightDown=keyboardEventKeyMotion==Pressed}
                        kc           -> return game
            _ -> return game

gameStep :: Game -> Game
gameStep game@(Game _ _ _ _ screenw screenh leftDown rightDown
           bat@(Bat batx baty batvx batvy batmaxspeed bataccel batw bath)
           ball@(Ball ballx bally ballvx ballvy ballmaxspeed ballw ballh)) =
    game{bat=bat', ball=ball'
       -- ,running=bally < baty -- for profiling
        }
    where
      batvx' = if leftDown then (max (batvx-bataccel) (-batmaxspeed)) else batvx
      batvx'' = if rightDown then (min (batvx'+bataccel) (batmaxspeed)) else batvx'
      batvx''' = if (and [not leftDown, not rightDown]) then truncate(fromIntegral batvx'' * (1.0-batfriction)) else batvx''
      (batx',batvx'''') = incrementWithStop batx  batvx''' 0 (screenw-batw)
      (baty',batvy') = (screenh-bath-40, 0)
      bat' = bat{batx=batx',baty=baty',batvx=batvx''',batvy=batvy'}
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

incrementWithBounce :: CInt -> CInt -> CInt -> CInt -> (CInt, CInt)
incrementWithBounce val inc lo hi =
    let v = val + inc in
    if v < lo then (lo+(lo-v), -inc)
    else if v > hi then (hi-(v-hi), -inc)
         else (v,inc)

incrementWithStop :: CInt -> CInt -> CInt -> CInt -> (CInt, CInt)
incrementWithStop val inc lo hi =
    let v = val + inc in
    if v < lo then (lo, -inc)
    else if v > hi then (hi, -inc)
         else (v,inc)

gameDraw :: Game -> IO ()
gameDraw Game{renderer,bat=Bat{..},ball=Ball{..}} = do
      rendererDrawColor renderer $= black >> clear renderer
      fillRectangle renderer (V2 batx baty) (V2 (batx+batw) (baty+bath)) red
      fillRectangle renderer (V2 ballx bally) (V2 (ballx+ballw) (bally+ballh)) white
      present renderer

black = V4 0 0 0 255
red = V4 255 0 0 255
white = V4 255 255 255 255
