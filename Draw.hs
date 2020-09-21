{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Draw (
      module Draw,
      module SDL,
      module SDL.Primitive
)
where
import Data.Word (Word8)
import SDL hiding (trace)
import SDL.Primitive

black = V4 0 0 0 255 :: V4 Word8
red = V4 255 0 0 255 :: V4 Word8
white = V4 255 255 255 255 :: V4 Word8
