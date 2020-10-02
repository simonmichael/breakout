{-# LANGUAGE RecordWildCards #-}
module Util
  ( module Util,
    module Debug.Trace,
    module Foreign.C.Types,
    module Text.Pretty.Simple,
  )
where

import Data.Word (Word32)
import Debug.Trace
import Foreign.C.Types
import SDL hiding (trace)
import Text.Pretty.Simple (pPrint)

-- debugging

-- | trace (print on stdout at runtime) a showable expression
-- (for easily tracing in the middle of a complex expression)
strace :: Show a => a -> a
strace a = trace (show a) a

-- | labelled trace - like strace, with a label prepended
ltrace :: Show a => String -> a -> a
ltrace l a = trace (l ++ ": " ++ show a) a

-- | monadic trace - like ltrace, but works as a standalone line in a monad
mtrace :: (Monad m, Show a) => String -> a -> m a
mtrace l a = ltrace l a `seq` return a

-- | trace an expression using a custom show function
tracewith :: (a -> String) -> a -> a
tracewith f e = trace (f e) e

--

type Ticks = Word32
type Tick = Ticks

type Seconds = Double
type SDLTime = Seconds

type X = CInt
type Y = CInt
type W = CInt
type H = CInt

type Value = CInt
type Increment = CInt
type LowBound = CInt
type HighBound = CInt

-- Add a signed increment to a value. If the lower or upper bound is reached,
-- by this call: reflect off the bound, change the increment's sign, 
-- and return a true "reached bound" flag.
-- All arguments are CInt, callers should take care to avoid wrapping.
incrementWithBounce :: Value -> Increment -> LowBound -> HighBound -> (Value, Increment, Bool)
incrementWithBounce vold inc lo hi = (v', inc', reachedbound)
    where
      v = vold + inc
      (v', inc') = 
            if v < lo then (lo+(lo-v), -inc)
            else if v > hi then (hi-(v-hi), -inc)
                           else (v,inc)
      reachedbound = inc' /= inc

-- Add a signed increment to a value. If the lower or upper bound is reached,
-- by this call: stop at the bound, change the increment to zero, 
-- and return a true "reached bound" flag.
-- All arguments are CInt, callers should take care to avoid wrapping.
incrementWithStop :: Value -> Increment -> LowBound -> HighBound -> (Value, Increment, Bool)
incrementWithStop vold inc lo hi = (v', inc', reachedbound)
    where
      v = vold + inc
      (v', inc') = 
            if v <= lo then (lo, 0)
            else if v >= hi then (hi,  0)
                            else (v, inc)
      reachedbound = inc' /= inc && v' /= vold

-- sdl

isKeyDn :: Event -> Keycode -> Bool
isKeyDn event keycode = eventIsKey event keycode Pressed

isKeyUp :: Event -> Keycode -> Bool
isKeyUp event keycode = eventIsKey event keycode Released

eventIsKey :: Event -> Keycode -> InputMotion -> Bool
eventIsKey event keycode keymotion =
  case eventPayload event of
    KeyboardEvent KeyboardEventData{..} -> and [
      keyboardEventKeyMotion == keymotion,
      keysymKeycode keyboardEventKeysym == keycode,
      not keyboardEventRepeat
      ]
    _ -> False

eventHasAnyKeyModifier :: Event -> [KeyModifier -> Bool] -> Bool
eventHasAnyKeyModifier event keymodaccessors =
  case eventPayload event of
    KeyboardEvent KeyboardEventData{..} -> any ($ eventmods) keymodaccessors
      where eventmods = keysymModifier keyboardEventKeysym
    _ -> False

eventHasShift :: Event -> Bool
eventHasShift = flip eventHasAnyKeyModifier [keyModifierLeftShift,keyModifierRightShift]

eventHasCtrl :: Event -> Bool
eventHasCtrl = flip eventHasAnyKeyModifier [keyModifierLeftCtrl,keyModifierRightCtrl]

eventHasAlt :: Event -> Bool
eventHasAlt = flip eventHasAnyKeyModifier [keyModifierLeftAlt,keyModifierRightAlt]

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

pickWith :: [a] -> Int -> a
pickWith as n = as !! (n `mod` length as)

