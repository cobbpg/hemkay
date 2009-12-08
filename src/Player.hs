module Player where

import Control.Monad
import Data.List
import Data.Maybe
import Sound.PortAudio
import Text.Printf

import Music

basePeriod :: Float
basePeriod = 3546894.6

baseFrequency :: Float
baseFrequency = 44100

playModule song = withDefaultStream 0 1 paFloat32 (realToFrac baseFrequency) 0x10000 $ \stream _ -> do
  forM_ (mixSong song) $ \(state,chunk) -> print state >> writeStream stream chunk 1

data PlayState = PS
                 { psTempo :: Int
                 , psBPM :: Int
                 , psChannels :: [ChannelState]
                 }

instance Show PlayState where
  show (PS t b cs) = printf "%d/%d %s" t b (show cs)

data ChannelState = CS
                    { csWaveData :: WaveData
                    , csPitch :: Int
                    , csFineTune :: Float
                    , csSubSample :: Float
                    , csSampleStep :: Float
                    , csVolume :: Float
                    , csInstrument :: Instrument
                    , csNextInstrument :: Instrument
                    , csEffect :: [Effect]
                    , csLastPortaDown :: Int
                    , csLastPortaUp :: Int
                    , csLastVolumeSlide :: Float
                    }

instance Show ChannelState where
  show cs = printf "<%d %f %s>" (csPitch cs) (csVolume cs) (show (csEffect cs))

startState song = PS { psTempo = 6
                     , psBPM = 125
                     , psChannels = replicate cnum chn
                     }
  where cnum = length.head.head.patterns $ song        
        chn = CS { csWaveData = []
                 , csPitch = 0
                 , csFineTune = 1
                 , csSubSample = 0
                 , csSampleStep = 0
                 , csVolume = 1
                 , csInstrument = undefined
                 , csNextInstrument = undefined
                 , csEffect = []
                 , csLastPortaDown = 0
                 , csLastPortaUp = 0
                 , csLastVolumeSlide = 0
                 }

{-
    ChannelNtPitch  : Array[1..8] Of Word;
    DelayChannel    : Byte;
    PatternLoopCnt  : Byte;
    PatternLoopRow  : Byte;
    PatternLoop     : Boolean;
    Panning         : Array[1..8] Of ShortInt;
    Surround        : Array[1..8] Of Boolean;
    LastVolumeSlide : Array[1..8] Of Byte;
    LastFineSlide   : Array[1..8] Of Byte;
    LastFinePorta   : Array[1..8] Of Byte;
    LastTonePorta   : Array[1..8] Of Byte;
    TonePortaEnd    : Array[1..8] Of Word;
    LastVibrato     : Array[1..8] Of Byte;
    LastTremolo     : Array[1..8] Of Byte;
    VibratoTable    : WavePtr;
    TremoloTable    : WavePtr;
    VibratoCounter  : Array[1..8] Of Byte;
    TremoloCounter  : Array[1..8] Of Byte;
    NewOrder, NewRow: Byte;
    CurrentNote     : Array[1..4] Of Byte;
    AbleJumps       : Boolean;
    BPStack         : Word;
-}

mixSong song = tail $ scanl mkChunk (startState song,[]) (concat (patterns song))
  where mkChunk (ps,_) line = (ps'',chunk)
          where ps' = foldl' processNote ps $ zip [0..] line
                (chunk,ps'') = mixChunk ps'
                processNote ps (i,Note per ins eff) = ps { psTempo = tempo'
                                                         , psBPM = bpm'
                                                         , psChannels = wrt i (psChannels ps) chn''
                                                         }
                  where tempo' = case eff of { [SetTempo x] -> x ; _ -> psTempo ps }
                        bpm' = case eff of { [SetBPM x] -> x ; _ -> psBPM ps }
                        chn = psChannels ps !! i
                        ins' = if ins /= 0 then instruments song !! (ins-1) else csInstrument chn
                        chn' = if per == 0 then chn
                               else chn { csWaveData = drop (case eff of { [SampleOffset o] -> o; _ -> 0 }) (wave ins')
                                        , csPitch = per
                                        , csFineTune = fineTune ins'
                                        , csSubSample = 0
                                        , csSampleStep = sampleStep per (fineTune ins')
                                        , csVolume = volume ins'
                                        , csInstrument = ins'
                                        }
                        chn'' = chn' { csEffect = eff
                                     , csVolume = case eff of { [SetVolume v] -> v; _ -> csVolume chn' }
                                     }
        wrt i xs e = let (pre,_:post) = splitAt i xs in pre ++ e:post

mixChunk ps = (concatMap snd chunks, fst (last chunks))
  where chunks = processTicks (psTempo ps) ps True
        tickLength = round (baseFrequency*2.5) `div` psBPM ps
        chnFact = 1 / fromIntegral (length (psChannels ps))
        processTicks 0    _  _       = []
        processTicks tick ps isFirst = (ps',mixResult) : processTicks (tick-1) ps' False
          where mixResult = map (\s -> [chnFact*s]) mixedChannels
                mixedChannels = foldl' addChannel (replicate tickLength 0) (psChannels ps)
                
                addChannel mix cs = mixChannel mix (csWaveData cs) (csSubSample cs)
                  where step = csSampleStep cs
                        vol = csVolume cs
                        mixChannel []     wd smp = []
                        mixChannel (m:ms) wd smp = wsmp `seq` wsmp : mixChannel ms wd' smp'
                          where wsmp = m + if null wd then 0 else head wd*vol
                                (wd',smp') = adv wd (smp+step) -- because properFraction is too slow
                                adv [] s = ([],s)
                                adv ws s | s >= 1    = adv (tail ws) (s-1)
                                         | otherwise = (ws,s)
                
                ps' = ps { psChannels = cs'' }
                cs' = map advChannel (psChannels ps)
                advChannel cs = cs { csWaveData = drop wdstep (csWaveData cs)
                                   , csSubSample = smp'
                                   }
                  where (wdstep,smp') = properFraction (csSubSample cs+csSampleStep cs*fromIntegral tickLength)
                cs'' = if isFirst then cs' else flip map cs' $ \cs -> case csEffect cs of
                  [Portamento (Porta p)] -> let pitch = clampPitch (csPitch cs + p) in
                    cs { csPitch = pitch
                       , csSampleStep = sampleStep pitch (csFineTune cs)
                       , csLastPortaDown = if p > 0 then p else csLastPortaDown cs
                       , csLastPortaUp = if p < 0 then p else csLastPortaUp cs }
                  [Portamento LastUp] -> let pitch = clampPitch (csPitch cs + csLastPortaUp cs) in
                    cs { csPitch = pitch
                       , csSampleStep = sampleStep pitch (csFineTune cs)
                       }
                  [Portamento LastDown] -> let pitch = clampPitch (csPitch cs + csLastPortaDown cs) in
                    cs { csPitch = pitch
                       , csSampleStep = sampleStep pitch (csFineTune cs)
                       }
                  [VolumeSlide x] -> let slide = fromMaybe (csLastVolumeSlide cs) x in 
                    cs { csVolume = max 0 $ min 1 $ csVolume cs + slide
                       , csLastVolumeSlide = slide
                       }
                  _ -> cs
                
sampleStep per ft = basePeriod / (fromIntegral per * baseFrequency) * ft

clampPitch = min 1712 . max 57
