import Sound.PortAudio
import System.Environment

import Loader
import Player

main = do
  initialize
  args <- getArgs
  song <- loadModule (head args)
  playModule song
  terminate
