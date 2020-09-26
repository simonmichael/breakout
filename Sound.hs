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


type Sound = Chunk

data Sounds = Sounds {
   sndballBat
  ,sndballWall
  ,sndballLoss
  ,sndbatWall
  :: Sound
} deriving Show

numchannels = 256

withSounds :: (Sounds -> IO a) -> IO a
withSounds f =
  withAudio defaultAudio 1000 $ do
    setChannels numchannels
    sndballBat   <- decode $(embedFile "data/paddle.wav")
    sndballWall  <- decode $(embedFile "data/wall.wav")
    sndballLoss  <- decode $(embedFile "data/hit.wav")
    sndbatWall   <- decode $(embedFile "data/Kick-Drum-1.wav")
    f Sounds{..}

-- Safe version of play that drops the sound instead of crashing
-- if no channels are available.
play' :: Sound -> IO ()
play' snd = do
  -- XXX perhaps racy
  channelavailable <- isJust <$> getAvailable DefaultGroup
  -- when (not channelavailable) $ mtrace "no sound channel" ()
  when channelavailable $ play snd
  