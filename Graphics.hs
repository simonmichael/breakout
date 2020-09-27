{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics (
      module Graphics,
      module SDL,
      module SDL.Font,
      module SDL.Primitive
)
where
import Data.Word (Word8)
import Data.FileEmbed
import SDL hiding (trace)
import qualified SDL.Font
import SDL.Font hiding (Mono,Normal,Color,initialize,quit,version)
import SDL.Primitive
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import Foreign.C (CInt)
import Util

black = V4 0 0 0 255 :: Color
red = V4 255 0 0 255 :: Color
white = V4 255 255 255 255 :: Color

clearWith :: MonadIO m => Renderer -> Color -> m ()
clearWith renderer color = rendererDrawColor renderer $= color >> clear renderer

goodTimesFont :: Int -> IO Font
goodTimesFont = decode $(embedFile "data/good-times.ttf")

data Fonts = Fonts {
  fnt1, fnt2, fnt3 :: Font
} deriving Show

withFonts :: (Fonts -> IO a) -> IO a
withFonts f = do
  SDL.Font.initialize
  fnt1 <- goodTimesFont 16
  fnt2 <- goodTimesFont 32
  fnt3 <- goodTimesFont 48
  r <- f Fonts{..}
  free fnt1
  free fnt2
  free fnt3
  SDL.Font.quit
  return r

textSize :: MonadIO m => Font -> T.Text -> m (CInt,CInt)
textSize f t = do
  (tw, th) <- size f t
  return (fromIntegral tw, fromIntegral th)

drawTextCenteredAt :: MonadIO m => Renderer -> Font -> CInt -> V2 CInt -> Color -> T.Text -> m ()
drawTextCenteredAt renderer font scale (V2 x y) color t = do
  (tw, th) <- both (scale*) <$> textSize font t
  let 
    tx = x - tw `div` 2
    ty = y - th `div` 2
    rect = Rectangle (P $ V2 tx ty) (V2 tw th)
  drawText renderer font color rect t

-- XXX creates a new surface/texture every time, should cache or pregenerate these
drawText :: MonadIO m => Renderer -> Font -> Color -> Rectangle CInt -> T.Text -> m ()
drawText renderer font color rect t = do
  s <- blended font color t
  t <- createTextureFromSurface renderer s
  copy renderer t Nothing (Just rect)
  freeSurface s
  destroyTexture t

