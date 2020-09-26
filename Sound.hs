{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Sound (
  module Sound,
  module SDL.Mixer
)
where
import Data.FileEmbed
import SDL.Mixer
import Control.Monad (when)

import Util
import Data.Maybe (isJust)

paddleSound :: IO Sound
paddleSound = decode $(embedFile "data/paddle.wav")

wallSound :: IO Sound
wallSound = decode $(embedFile "data/wall.wav")

hitSound :: IO Sound
hitSound = decode $(embedFile "data/hit.wav")

kickdrumSound :: IO Sound
kickdrumSound = decode $(embedFile "data/Kick-Drum-1.wav")

type Sound = Chunk

data Sounds = Sounds {
   sndpaddle
  ,sndwall
  ,sndhit
  ,sndkickdrum

  :: Sound
} deriving Show

numchannels = 256

withSounds :: (Sounds -> IO a) -> IO a
withSounds f =
  withAudio defaultAudio 1000 $ do
    setChannels numchannels
    sndpaddle    <- paddleSound
    sndwall      <- wallSound
    sndhit       <- hitSound
    sndkickdrum  <- kickdrumSound
    f Sounds{..}

-- Safe version of play that drops the sound instead of crashing
-- if no channels are available.
play' :: Sound -> IO ()
play' snd = do
  -- XXX perhaps racy
  channelavailable <- isJust <$> getAvailable DefaultGroup
  -- when (not channelavailable) $ mtrace "no sound channel" ()
  when channelavailable $ play snd
  