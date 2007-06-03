module Main where

import Debug.Trace
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as Image
import Graphics.UI.SDL.Framerate as Framerate
import Foreign
import Data.Typeable
import Data.Char
import Data.IORef
import Control.Monad
import System.Environment
import System.Exit
import System.Random

screenw     = 640
screenh     = 480
screendepth = 16
framerate   = 40 -- (hz)

data Game = Game {
      running :: Bool,
      fpsmgr :: FPSManager,
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
      batstep :: Int,
      batw :: Int,
      bath :: Int
}
data Ball = Ball {
      ballx :: Int,
      bally :: Int,
      ballvx :: Int,
      ballvy :: Int,
      ballstep :: Int,
      ballw :: Int,
      ballh :: Int
}

newGame fpsmgr = Game True fpsmgr False False newBat newBall
newBat  = Bat (div screenw 2) (screenh-h-30) 0 0 3 w h where w = 60; h = 10
newBall = Ball 0 0 2 2 2 10 10

main = initialize >>= mainloop
 
initialize :: IO (IORef Game)
initialize =
    do
      SDL.init [InitVideo]
      setVideoMode screenw screenh screendepth [SWSurface]
      setCaption "Breakout" ""
      enableUnicode True
      fpsmgr <- Framerate.new
      Framerate.init fpsmgr
      Framerate.set fpsmgr framerate
      gameref <- newIORef $ newGame fpsmgr
      return gameref

mainloop gameref =
    do 
      game <- readIORef gameref
      game' <- getinput game
      let game = step game'
      display game
      Framerate.delay $ fpsmgr game
      writeIORef gameref game
      when (running game) (do mainloop gameref)

getinput game =
    do
      event <- pollEvent
      return $ case event of
            KeyDown (Keysym SDLK_q _ _)     -> game{running=False}
            Quit                            -> game{running=False}
            KeyDown (Keysym SDLK_LEFT _ _)  -> game{leftDown=True}
            KeyUp   (Keysym SDLK_LEFT _ _)  -> game{leftDown=False}
            KeyDown (Keysym SDLK_RIGHT _ _) -> game{rightDown=True}
            KeyUp   (Keysym SDLK_RIGHT _ _) -> game{rightDown=False}
            otherwise                       -> game

step game@(Game _ _ leftDown rightDown
           bat@(Bat batx baty batvx batvy batstep batw bath)
           ball@(Ball ballx bally ballvx ballvy ballstep ballw ballh)) =
    game{bat=bat', ball=ball'}
    where
      batvx' = if leftDown then (-batstep) else 0
      batvx'' = if rightDown then (batvx'+batstep) else batvx'
      (batx',batvx''') = incrementWithBounce batx  batvx'' 0 (screenw-batw)
      (baty',batvy')   = incrementWithBounce baty  batvy   0 (screenh-bath)
      bat' = bat{batx=batx',baty=baty',batvx=batvx''',batvy=batvy'}
      (ballx',ballvx') = incrementWithBounce ballx ballvx  0 (screenw-ballw)
      (bally',ballvy') = if (and [ballx >= batx-ballw, 
                                  ballx <= (batx+batw), 
                                  bally >= (baty-ballh), 
                                  bally <= baty,
                                  ballvy > 0])
                         then incrementWithBounce bally ballvy 0 (baty-ballh)
                         else incrementWithBounce bally ballvy 0 (screenh-ballh)
      ball' = ball{ballx=ballx',bally=bally',ballvx=ballvx',ballvy=ballvy'}

incrementWithBounce :: Int -> Int -> Int -> Int -> (Int, Int)
incrementWithBounce val inc lo hi =
    let v = val + inc in
    if v < lo then (lo+(lo-v), -inc)
    else if v > hi then (hi-(v-hi), -inc)
         else (v,inc)

display (Game _ _ _ _
         (Bat batx baty _ _ _ batw bath)
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

