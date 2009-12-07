module Player where

import Sound.PortAudio

import Music

playModule song = withDefaultStream 0 1 paFloat32 44100 256 $ \stream _ -> do
  writeStream stream [map sin [0,0.01..500]] 1
  paSleep 2000
