{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Draw (
      module Draw,
      module SDL,
      module SDL.Font,
      module SDL.Primitive
)
where
import Data.Word (Word8)
import SDL hiding (trace)
import qualified SDL.Font
import SDL.Font hiding (Mono,Normal,Color,initialize,quit,version)
import SDL.Primitive

black = V4 0 0 0 255 :: V4 Word8
red = V4 255 0 0 255 :: V4 Word8
white = V4 255 255 255 255 :: V4 Word8

data Fonts = Fonts {
   fntcrystal
  :: Font
}

withFonts :: (Fonts -> IO a) -> IO a
withFonts f = do
  SDL.Font.initialize
  fntcrystal <- load "data/Crysta.ttf" 16
  r <- f Fonts{..}
  free fntcrystal
  SDL.Font.quit
  return r

