import Sound.PortAudio.MB
import System.Environment

import Loader
import Player

main :: IO ()
main = do
  initialize
  args <- getArgs
  song <- loadModule (head args)
  playModule song
  terminate
