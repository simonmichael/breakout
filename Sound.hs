{-# LANGUAGE RecordWildCards #-}
module Sound (
  module Sound,
  module SDL.Mixer
)
where
import SDL.Mixer
import Control.Monad (when)

import Util
import Data.Maybe (isJust)

type Sound = Chunk

data Sounds = Sounds {
   sndding
  ,sndbassdrum1
  ,sndkickdrum1
  ,sndpaddle
  ,sndwall
  :: Sound
}

numchannels = 256

withSounds :: (Sounds -> IO a) -> IO a
withSounds f =
  withAudio defaultAudio 1000 $ do
    setChannels numchannels
    sndding <- load "data/haskanoid/196106_aiwha_ding-cc-by.wav"
    sndbassdrum1 <- load "data/Bass-Drum-1.wav"
    sndkickdrum1 <- load "data/Kick-Drum-1.wav"
    sndpaddle <- load "data/paddle.wav"
    sndwall <- load "data/wall.wav"
    f Sounds{..}

-- Safe version of play that drops the sound instead of crashing
-- if no channels are available.
play' :: Sound -> IO ()
play' snd = do
  -- XXX perhaps racy
  channelavailable <- isJust <$> getAvailable DefaultGroup
  -- when (not channelavailable) $ mtrace "no sound channel" ()
  when channelavailable $ play snd
  