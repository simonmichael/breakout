module Util
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

-- Add a signed increment to a value, reflecting off the lower or upper bound
-- (and negating the increment, and returning a true bounced flag) if they are reached.
incrementWithBounce :: Value -> Increment -> LowBound -> HighBound -> (Value, Increment, Bool)
incrementWithBounce val inc lo hi =
    let v = val + inc in
    if v < lo then (lo+(lo-v), -inc, True)
    else if v > hi then (hi-(v-hi), -inc, True)
         else (v,inc, False)

-- Add a signed increment to a value, stopping at the lower or upper bound
-- (and negating the increment, and returning a true stopped flag) if they are reached.
incrementWithStop :: Value -> Increment -> LowBound -> HighBound -> (Value, Increment, Bool)
incrementWithStop val inc lo hi =
    let v = val + inc in
    if v < lo then (lo, -inc, True)
    else if v > hi then (hi, -inc, True)
         else (v,inc, False)

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

