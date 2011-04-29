{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}

import Control.Concurrent
import Control.Monad
import Control.Monad.Fix
import Data.Int
import Data.List
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Sound.Hemkay.Loader
import Sound.Hemkay.Mixer
import Sound.OpenAL
import System.Environment

bufferSize :: Int
bufferSize = 16384

main :: IO ()
main = do
  args <- getArgs
  song <- loadModule (head args)
  devs <- get allDeviceSpecifiers
  -- OSS seems to be a lot faster than ALSA, so we prefer it
  mdev <- openDevice (find (isPrefixOf "OSS") devs)
  case mdev of
    Nothing -> putStrLn "Could not open sound device." >> return False
    Just dev -> do
      mctx <- createContext dev [Frequency sampleFrequency, StereoSources 1]
      currentContext $= mctx
      case mctx of
        Nothing -> putStrLn "Could not create sound context."
        Just ctx -> do
          [src] <- genObjectNames 1
          bufs@[buf1,buf2] <- genObjectNames 2
          ptrs <- replicateM 2 (mallocArray (bufferSize*2) :: IO (Ptr Int16))
          mem <- mallocArray (bufferSize*2) :: IO (Ptr Float)
          let fillBuffer n = do
                let ptr = ptrs !! n
                    fill i = do
                      f <- peekElemOff mem i
                      pokeElemOff ptr i (fromIntegral (truncate (f*32767) :: Int))
                      when (i >= 0) $ fill (i-1)
                fill (bufferSize*2)
                bufferData (bufs !! n) $= BufferData (MemoryRegion ptr (fromIntegral bufferSize*4)) Stereo16 sampleFrequency

              mixFun = mixToBuffer mem bufferSize
              mst = map prepareMix (performSong song)

          Just mst' <- mixFun mst
          fillBuffer 0
          Just mst'' <- mixFun mst'
          fillBuffer 1
          queueBuffers src bufs

          play [src]
          flip fix (mst'',True) $ \loop (mst,isFst) -> do
            x <- get (buffersProcessed src)
            next <- case x of
              1 -> do
                unqueueBuffers src [if isFst then buf1 else buf2]
                Just mst' <- mixFun mst
                fillBuffer (if isFst then 0 else 1)
                queueBuffers src [if isFst then buf1 else buf2]
                return (mst',not isFst)
              _ -> return (mst,isFst)
            threadDelay 100000
            loop next
          stop [src]

          deleteObjectNames [src]
          deleteObjectNames bufs
          currentContext $= Nothing
          destroyContext ctx
      closeDevice dev
  return ()
