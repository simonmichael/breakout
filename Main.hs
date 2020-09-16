-- breakout prototype in haskell using SDL


module Main where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as Image
import Graphics.UI.SDL.Framerate as Framerate
import Graphics.UI.SDL.Mixer as Mixer
import Foreign
import Data.Typeable
import Data.Char
import Data.IORef
import Control.Monad
import System.Environment
import System.Exit
import System.Random

-- debugging
import Debug.Trace

-- | trace (print on stdout at runtime) a showable expression
-- (for easily tracing in the middle of a complex expression)
strace :: Show a => a -> a
strace a = trace (show a) a

-- | labelled trace - like strace, with a label prepended
ltrace :: Show a => String -> a -> a
ltrace l a = trace (l ++ ": " ++ show a) a

-- | monadic trace - like strace, but works as a standalone line in a monad
mtrace :: (Monad m, Show a) => a -> m a
mtrace a = strace a `seq` return a

-- | trace an expression using a custom show function
tracewith f e = trace (f e) e

--

screenwidth  = 400
screenheight = 400
screendepth = 32
screenmode = [SWSurface,DoubleBuf,Resizable]

-- screenwidth  = 1680
-- screenheight = 1050
-- screendepth = 32
-- screenmode = [HWSurface,DoubleBuf,Fullscreen]

framerate   = 60 -- (hz)
batfriction = 0.2
defbataccel = 2

data Game = Game {
      running :: Bool,
      fpsmgr :: FPSManager,
      screenw :: Int,
      screenh :: Int,
      leftDown :: Bool,
      rightDown :: Bool,
      bat :: Bat,
      ball :: Ball
}
data Bat = Bat {
      batx :: Int,
      baty :: Int,
      batvx :: Int,
      batvy :: Int,
      batmaxspeed :: Int,
      bataccel :: Int,
      batw :: Int,
      bath :: Int
}
data Ball = Ball {
      ballx :: Int,
      bally :: Int,
      ballvx :: Int,
      ballvy :: Int,
      ballmaxspeed :: Int,
      ballw :: Int,
      ballh :: Int
}

main :: IO ()
main = initialize >>= mainloop
 
initialize :: IO Game
initialize =
    do
      SDL.init [InitVideo,InitAudio]
      setVideoMode screenwidth screenheight screendepth screenmode
      setCaption "Breakout" ""
      enableUnicode True
      fpsmgr <- Framerate.new
      Framerate.init fpsmgr
      Framerate.set fpsmgr framerate
      return $ newGame fpsmgr

newGame :: FPSManager -> Game
newGame fpsmgr = Game True fpsmgr screenwidth screenheight False False newBat newBall
newBat  = Bat (div screenwidth 2) (screenheight-h-40) 0 0 10 defbataccel w h where w = 60; h = 10
newBall = Ball 0 0 1 1 0 8 8

mainloop :: Game -> IO ()
mainloop game = do
  game' <- pollEvent >>= handleevent game
  let game'' = step game'
  -- Framerate.delay $ fpsmgr game''
  display game''
  unless (not (running game'')) $ mainloop game''

handleevent :: Game -> Event -> IO Game
handleevent game (Quit)                            = return game{running=False}
handleevent game (KeyDown (Keysym SDLK_q _ _))     = return game{running=False}
handleevent game (KeyDown (Keysym SDLK_LEFT _ _))  = return game{leftDown=True}
handleevent game (KeyUp   (Keysym SDLK_LEFT _ _))  = return game{leftDown=False}
handleevent game (KeyDown (Keysym SDLK_RIGHT _ _)) = return game{rightDown=True}
handleevent game (KeyUp   (Keysym SDLK_RIGHT _ _)) = return game{rightDown=False}
handleevent game (VideoResize w h)                 = 
    do
      setVideoMode w h screendepth screenmode
      return game{screenw=w,screenh=h}
handleevent game _ = return game

step :: Game -> Game
step game@(Game _ _ screenw screenh leftDown rightDown
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

incrementWithBounce :: Int -> Int -> Int -> Int -> (Int, Int)
incrementWithBounce val inc lo hi =
    let v = val + inc in
    if v < lo then (lo+(lo-v), -inc)
    else if v > hi then (hi-(v-hi), -inc)
         else (v,inc)

incrementWithStop val inc lo hi =
    let v = val + inc in
    if v < lo then (lo, -inc)
    else if v > hi then (hi, -inc)
         else (v,inc)

display :: Game -> IO ()
display (Game _ _ _ _ _ _
         (Bat batx baty _ _ _ _ batw bath)
         (Ball ballx bally _ _ _ ballw ballh)) =
    do 
      screen <- getVideoSurface
      let format = surfaceGetPixelFormat screen
      red <- mapRGB format 0xFF 0 0
      green <- mapRGB format 0 0xFF 0
      black <- mapRGB format 0 0 0
      white <- mapRGB format 0xFF 0xFF 0xFF
      fillRect screen Nothing black
      fillRect screen (Just (Rect batx baty batw bath)) red
      fillRect screen (Just (Rect ballx bally ballw ballh)) white
      SDL.flip screen

