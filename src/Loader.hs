module Loader where

import Control.Applicative
import Control.Monad
import Data.Array.IO
import Data.Binary
import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString.Lazy as LS
import qualified Data.ByteString.Char8 as S
import Data.List
import Data.Maybe
import System.IO.Unsafe

import Music

formatList = [("M.K.",4),("M!K!",4),("FLT4",4),("FLT8",8),("4CHN",4),("6CHN",6),("8CHN",8)]
               
loadModule path = readModule <$> LS.readFile path

readModule = runGet $ do
  name <- getString 20
  sampleInfo <- replicateM 31 getSampleInfo
  songLength <- getByte
  skip 1
  orderList <- take songLength <$> replicateM 128 getByte
  numChannels <- flip lookup formatList <$> getString 4
  when (isNothing numChannels) (fail "Unknown format")
  patternData <- getPatterns (maximum orderList + 1) (fromJust numChannels)
  sampleData <- mapM getBytes $ map getSampleLength sampleInfo
  return $ Song
    { title = name
    , samples = map mkSample $ zip sampleInfo (map S.unpack sampleData)
    , patterns = map (patternData !!) orderList
    }

getString = fmap (takeWhile (/='\0') . S.unpack) . getByteString

getSize = fromIntegral . (*2) <$> getWord16be

getByte = fromIntegral <$> getWord8

getSampleInfo = (,,,,,) <$> getString 22 <*> getSize <*> getByte <*> getByte <*> getSize <*> getSize

getSampleLength (_,l,_,_,_,_) = l

getPatterns count chan = replicateM count (replicateM 64 (replicateM chan getNote))

getNote = do
  [n1,n2,n3,n4] <- replicateM 4 getWord8
  return $ Note
    { period = fromIntegral $ shift (n1 .&. 0xf) 8 .|. n2
    , instrument = fromIntegral $ (n1 .&. 0xf0) .|. shift n3 (-4)
    , effect = mkEffect (n3 .&. 0xf) (shift n4 (-4)) (fromIntegral n4 `mod` if n3 == 0xe then 16 else 256)
    }

mkEffect 0x0 _   0 = []
mkEffect 0x0 _   x = [Arpeggio (x `div` 16) (x `mod` 16)]
mkEffect 0x1 _   0 = [Portamento LastUp]
mkEffect 0x1 _   x = [Portamento (Porta x)]
mkEffect 0x2 _   0 = [Portamento LastDown]
mkEffect 0x2 _   x = [Portamento (Porta (-x))]
mkEffect 0x3 _   0 = [TonePortamento Nothing]
mkEffect 0x3 _   x = [TonePortamento (Just x)]
mkEffect 0x4 _   x = [uncurry Vibrato (mkWaveSpeed x)]
mkEffect 0x5 _   x = [TonePortamento Nothing, VolumeSlide (mkVolSlide x)]
mkEffect 0x6 _   x = [Vibrato Nothing Nothing, VolumeSlide (mkVolSlide x)]
mkEffect 0x7 _   x = [uncurry Tremolo (mkWaveSpeed x)]
mkEffect 0x8 _   x = [FinePanning x]
mkEffect 0x9 _   x = [SampleOffset (x*256)]
mkEffect 0xa _   x = [VolumeSlide (mkVolSlide x)]
mkEffect 0xb _   x = [OrderJump x]
mkEffect 0xc _   x = [SetVolume x]
mkEffect 0xd _   x = [PatternBreak x]
mkEffect 0xe 0x1 0 = [FinePortamento LastUp]
mkEffect 0xe 0x1 x = [FinePortamento (Porta x)]
mkEffect 0xe 0x2 0 = [FinePortamento LastDown]
mkEffect 0xe 0x2 x = [FinePortamento (Porta (-x))]
mkEffect 0xe 0x4 0 = [SetVibratoWaveform SineWave]
mkEffect 0xe 0x4 1 = [SetVibratoWaveform SawtoothWave]
mkEffect 0xe 0x4 2 = [SetVibratoWaveform SquareWave]
mkEffect 0xe 0x5 x = [FineTuneControl x]
mkEffect 0xe 0x6 0 = [PatternLoop Nothing]
mkEffect 0xe 0x6 x = [PatternLoop (Just x)]
mkEffect 0xe 0x7 0 = [SetTremoloWaveform SineWave]
mkEffect 0xe 0x7 1 = [SetTremoloWaveform SawtoothWave]
mkEffect 0xe 0x7 2 = [SetTremoloWaveform SquareWave]
mkEffect 0xe 0x8 x = [GravisPanning x]
mkEffect 0xe 0x9 0 = []
mkEffect 0xe 0x9 x = [RetrigNote x]
mkEffect 0xe 0xa 0 = [FineVolumeSlide Nothing]
mkEffect 0xe 0xa x = [FineVolumeSlide (Just x)]
mkEffect 0xe 0xb 0 = [FineVolumeSlide Nothing]
mkEffect 0xe 0xb x = [FineVolumeSlide (Just (-x))]
mkEffect 0xe 0xc 0 = []
mkEffect 0xe 0xc x = [NoteCut x]
mkEffect 0xe 0xd 0 = []
mkEffect 0xe 0xd x = [NoteDelay x]
mkEffect 0xe 0xe 0 = []
mkEffect 0xe 0xe x = [PatternDelay x]
mkEffect 0xf _   x = [if x < 32 then SetTempo x else SetBPM x]
mkEffect _   _   _ = []

mkVolSlide 0 = Nothing
mkVolSlide x = Just (x `div` 16 - x `mod` 16)

mkWaveSpeed x = let (a1,a2) = divMod x 16 in (notZero a1,notZero a2)
  where notZero 0 = Nothing
        notZero x = Just x

mkSample ((n,len,ft,vol,lbeg,llen),dat) =
             Sample { name = n
                    , wave = if llen <= 2 then dat'
                             else take lbeg dat' ++ cycle (take llen (drop lbeg dat'))
                    , volume = vol
                    , fineTune = ft
                    }
               where dat' = map charToFloat dat

charToFloat = unsafePerformIO . readArray floatSamples

floatSamples :: IOUArray Char Float
floatSamples = unsafePerformIO $ newListArray ('\0','\255') floats
  where floats = map (/128) ([0..127] ++ [-128..1])