module Types
where
import Foreign.C (CInt)
-- import SDL hiding (trace)
-- import qualified SDL.Framerate as Framerate

-- Our aspirational frame rate. We will show no more than this many frames per second,
-- enforced by Framerate.delay below. 
-- On mac at least, vsync (rendererType = AcceleratedVSyncRenderer below) also enforces
-- a max frame rate of 60 fps. 
framerate = 60 :: Int  -- fps

-- increase to multiply all speeds (default 1), but be careful not to overflow CInt
speedup = 2 :: CInt

defscreenwidth = 500 :: CInt
defscreenheight = 500 :: CInt

defballx = 0 :: CInt
defbally = defscreenheight `div` 2 - 80 :: CInt

