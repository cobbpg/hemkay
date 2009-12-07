import Sound.PortAudio

import Loader
import Player

main = do
  initialize
  song <- loadModule "rocks.mod"
  playModule song
  terminate
