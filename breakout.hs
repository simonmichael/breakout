-- breakout prototype in haskell using SDL

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

screenwidth  = 640
screenheight = 480
screendepth = 16
screenmode = [SWSurface,Resizable]
framerate   = 30 -- (hz)
batfriction = 0.1

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

-- some type sigs:
-- newGame :: FPSManager -> Game
-- newBat :: Bat
-- main :: IO ()
-- initialize :: IO Game
-- mainloop :: Game -> IO ()
-- getinput :: Game -> IO Game
-- step :: Game -> Game
-- incrementWithBounce :: Int -> Int -> Int -> Int -> (Int, Int)
-- display :: Game -> IO ()

newGame fpsmgr = Game True fpsmgr screenwidth screenheight False False newBat newBall
newBat  = Bat (div screenwidth 2) (screenheight-h-40) 0 0 10 1 w h where w = 60; h = 10
newBall = Ball 0 0 4 4 0 8 8

main = initialize >>= mainloop
 
initialize =
    do
      SDL.init [InitVideo]
      setVideoMode screenwidth screenheight screendepth screenmode
      setCaption "Breakout" ""
      enableUnicode True
      fpsmgr <- Framerate.new
      Framerate.init fpsmgr
      Framerate.set fpsmgr framerate
      return $ newGame fpsmgr

mainloop game =
    do 
      event <- pollEvent
      game' <- handleevent game event
      let game = step game'
      Framerate.delay $ fpsmgr game
      display game
      when (running game) (do mainloop game)

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

step game@(Game _ _ screenw screenh leftDown rightDown
           bat@(Bat batx baty batvx batvy batmaxspeed bataccel batw bath)
           ball@(Ball ballx bally ballvx ballvy ballmaxspeed ballw ballh)) =
    game{bat=bat', ball=ball'}
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

