module Util (
      module Util,
      module Debug.Trace
)
where
import Debug.Trace
import SDL hiding (trace)
import Foreign.C (CInt)

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
-- All arguments are CInt, so callers should take care to avoid wrapping.
incrementWithBounce :: Value -> Increment -> LowBound -> HighBound -> (Value, Increment, Bool)
incrementWithBounce vold inc lo hi = (v', inc', reachedbound)
    where
      v = vold + inc
      (v', inc', atbound) = 
            if v < lo then (lo+(lo-v), -inc, True)
            else if v > hi then (hi-(v-hi), -inc, True)
                           else (v,inc, False)
      reachedbound = atbound && v' /= vold

-- Add a signed increment to a value. If the lower or upper bound is reached,
-- by this call: stop at the bound, change the increment's sign, 
-- and return a true "reached bound" flag.
-- All arguments are CInt, so callers should take care to avoid wrapping.
incrementWithStop :: Value -> Increment -> LowBound -> HighBound -> (Value, Increment, Bool)
incrementWithStop vold inc lo hi = (v', inc', reachedbound)
    where
      v = vold + inc
      (v', inc', atbound) = 
            if v < lo then (lo, -inc, True)
            else if v > hi then (hi, -inc, True)
                           else (v,inc, False)
      reachedbound = atbound && v' /= vold

-- sdl

isKeyDn :: Event -> Keycode -> Bool
isKeyDn event keycode = eventIsKey event keycode Pressed

isKeyUp :: Event -> Keycode -> Bool
isKeyUp event keycode = eventIsKey event keycode Released

eventIsKey :: Event -> Keycode -> InputMotion -> Bool
eventIsKey event keycode keymotion = 
      case eventPayload event of
            KeyboardEvent keyboardEvent ->
                  keyboardEventKeyMotion keyboardEvent == keymotion &&
                  keysymKeycode (keyboardEventKeysym keyboardEvent) == keycode
            _ -> False

