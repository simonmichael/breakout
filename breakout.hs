module Main where

import Debug.Trace
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as Image
import Foreign
import Data.Typeable
import Data.Char
import Data.IORef
import Control.Monad
import System.Environment
import System.Exit
import System.Random

screenw = 640
screenh = 480
screendepth = 16

data Game = Game {
      running :: Bool,
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

newGame = Game True False False newBat newBall
newBat  = Bat (div screenw 2) (screenh-h-30) 0 0 3 w h where w = 60; h = 10
newBall = Ball 0 0 2 2 2 10 10

main =
    do 
      -- init
      SDL.init [InitVideo]
      screen <- setVideoMode screenw screenh screendepth [SWSurface]
      setCaption "Breakout" ""
      enableUnicode True
      gameref <- newIORef newGame
      -- main loop
      mainloop gameref

mainloop gameref =
    do 
      delay 5
      event <- pollEvent
      game@(Game _ l r 
            (Bat batx baty batvx batvy batstep batw bath)
            (Ball ballx bally ballvx ballvy ballstep ballw ballh)) <- readIORef gameref
      let game = case event of
            KeyDown (Keysym SDLK_q _ _)     -> Game False l     r     (Bat batx baty 0     0     batstep batw bath) (Ball ballx bally ballvx ballvy ballstep ballw ballh)
            Quit                            -> Game False l     r     (Bat batx baty 0     0     batstep batw bath) (Ball ballx bally ballvx ballvy ballstep ballw ballh)
            KeyDown (Keysym SDLK_LEFT _ _)  -> Game True  True  r     (Bat batx baty batvx batvy batstep batw bath) (Ball ballx bally ballvx ballvy ballstep ballw ballh)
            KeyUp   (Keysym SDLK_LEFT _ _)  -> Game True  False r     (Bat batx baty batvx batvy batstep batw bath) (Ball ballx bally ballvx ballvy ballstep ballw ballh)
            KeyDown (Keysym SDLK_RIGHT _ _) -> Game True  l     True  (Bat batx baty batvx batvy batstep batw bath) (Ball ballx bally ballvx ballvy ballstep ballw ballh)
            KeyUp   (Keysym SDLK_RIGHT _ _) -> Game True  l     False (Bat batx baty batvx batvy batstep batw bath) (Ball ballx bally ballvx ballvy ballstep ballw ballh)
            otherwise                       -> Game True  l     r     (Bat batx baty batvx batvy batstep batw bath) (Ball ballx bally ballvx ballvy ballstep ballw ballh)
      let game' = step game
      display game'
      writeIORef gameref game'
      when (running game') (do mainloop gameref)

step (Game running l r 
            (Bat batx baty batvx batvy batstep batw bath)
            (Ball ballx bally ballvx ballvy ballstep ballw ballh)) =
    Game running l r 
         (Bat batx' baty' batvx''' batvy' batstep batw bath)
         (Ball ballx' bally' ballvx' ballvy' ballstep ballw ballh)
    where
      batvx' = if l then (-batstep) else 0
      batvx'' = if r then (batvx'+batstep) else batvx'
      (batx',batvx''') = incrementWithBounce batx  batvx'' 0 (screenw-batw)
      (baty',batvy')   = incrementWithBounce baty  batvy   0 (screenh-bath)
      (ballx',ballvx') = incrementWithBounce ballx ballvx  0 (screenw-ballw)
      (bally',ballvy') = if (and [ballx >= batx-ballw, 
                                  ballx <= (batx+batw), 
                                  bally >= (baty-ballh), 
                                  bally <= baty,
                                  ballvy > 0])
                         then incrementWithBounce bally ballvy 0 (baty-ballh)
                         else incrementWithBounce bally ballvy 0 (screenh-ballh)

incrementWithBounce :: Int -> Int -> Int -> Int -> (Int, Int)
incrementWithBounce val inc lo hi =
    let v = val + inc in
    if v < lo then (lo+(lo-v), -inc)
    else if v > hi then (hi-(v-hi), -inc)
         else (v,inc)

display (Game running l r 
            (Bat batx baty batvx batvy batstep batw bath)
            (Ball ballx bally ballvx ballvy ballstep ballw ballh)) =
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

