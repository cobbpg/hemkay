import Control.Monad
import Control.Monad.Fix
import Sound.Hemkay.Loader
import Sound.Hemkay.Mixer
import Sound.PortAudio hiding (chunk)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  song <- loadModule (head args)
  withPortAudio $
    withDefaultStream 0 2 paFloat32 (realToFrac sampleFrequency) 0x1000 $ \stream _ -> do
      flip fix (mixSong song) $ \playback chunks -> unless (null chunks) $ do
        let (state,chunk):songRest = chunks
        flip fix chunk $ \push chk -> unless (null chk) $ do
          Right len <- getStreamWriteAvailable stream
          let (pre,rest) = splitAt len chk
          writeStream stream (map (\(Smp sl sr) -> [sl,sr]) pre) len
          push rest
        putStr (show state)
        playback songRest
  return ()