import Control.Concurrent.MVar
import Data.IORef
import Foreign.Ptr
import Sound.Hemkay.Loader
import Sound.Hemkay.Mixer
import Sound.PortAudio
import System.Environment

bufLength :: Int
bufLength = 0x1000

main :: IO ()
main = do
  args <- getArgs
  song <- loadModule (head args)
  songRef <- newIORef . map prepareMix . performSong $ song
  sync <- newEmptyMVar
  withPortAudio $ withDefaultStream 0 2 [Float32] (realToFrac sampleFrequency) bufLength
    (Just (feedSong songRef sync)) (const (takeMVar sync))
  return ()

feedSong :: IORef SongMixState -> MVar () -> Ptr Float -> Ptr Float -> FrameCount
            -> Maybe StreamCallbackTimeInfo -> StreamCallbackFlags -> IO StreamCallbackResult
feedSong ref sync _ outPtr _ _ _ = do
  mixState <- readIORef ref
  res <- mixToBuffer outPtr bufLength mixState
  case res of
    Nothing -> do
      putMVar sync ()
      return Complete
    Just mixState' -> do
      writeIORef ref mixState'
      return Continue
