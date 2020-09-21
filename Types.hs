module Types
where
import Foreign.C (CInt)
-- import SDL hiding (trace)
-- import qualified SDL.Framerate as Framerate

-- increase to multiply the speed of everything (default 1),
-- but be careful not to overflow CInt
speedup :: CInt
speedup = 1