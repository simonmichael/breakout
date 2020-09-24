{-# LANGUAGE OverloadedStrings #-}
module Constants
where
import qualified Data.Text as T
import Foreign.C (CInt)
-- import SDL hiding (trace)
-- import qualified SDL.Framerate as Framerate
import Util ( Seconds )

progname = "sm-breakout" :: T.Text

-- Our aspirational frame rate. We will show no more than this many frames per second,
-- enforced by Framerate.delay below. 
-- On mac at least, vsync (rendererType = AcceleratedVSyncRenderer below) also enforces
-- a max frame rate of 60 fps. 
framerate = 60 :: Int  -- fps

-- increase to multiply all speeds (default 1), but be careful not to overflow CInt
speedup = 2 :: CInt

gameoverdelay = 5 :: Seconds

defscreenwidth = 500 :: CInt
defscreenheight = 500 :: CInt

defbatfriction = 0.2  -- not much effect right now
defbatmaxspeed = 4 :: CInt
defbataccel = 1 :: CInt
defbatwidth = 60 :: CInt
defbatheight = 10 :: CInt

defballx = 0 :: CInt
defbally = defscreenheight `div` 2 - 80 :: CInt
defballspeed = 4 :: CInt
defballwidth = 10 :: CInt
defballheight = 10 :: CInt
