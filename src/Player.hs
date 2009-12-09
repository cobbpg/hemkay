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
  forM_ (mixSong song) $ \(state,chunk) -> print state >> forM_ chunk (\x -> writeStream stream [x] 1)

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
                    , csLastFinePorta :: Int
                    , csTonePortaEnd :: Int
                    , csLastTonePorta :: Int
                    , csLastVolumeSlide :: Float
                    , csLastFineSlide :: Float
                    }

instance Show ChannelState where
  show cs = printf "<%s %2d %s>" (periodName (csPitch cs)) (round (csVolume cs*99) :: Int) (show (csEffect cs))

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
                 , csLastFinePorta = 0
                 , csTonePortaEnd = 0
                 , csLastTonePorta = 0
                 , csLastVolumeSlide = 0
                 , csLastFineSlide = 0
                 }

{-
  State vars still missing:

  DelayChannel    : Byte;
  PatternLoopCnt  : Byte;
  PatternLoopRow  : Byte;
  PatternLoop     : Boolean;
  Panning         : Array[1..8] Of ShortInt;
  Surround        : Array[1..8] Of Boolean;
  LastVibrato     : Array[1..8] Of Byte;
  LastTremolo     : Array[1..8] Of Byte;
  VibratoTable    : WavePtr;
  TremoloTable    : WavePtr;
  VibratoCounter  : Array[1..8] Of Byte;
  TremoloCounter  : Array[1..8] Of Byte;
  NewOrder, NewRow: Byte;
  CurrentNote     : Array[1..4] Of Byte;
-}

mixSong song = tail $ scanl mkChunk (startState song,[]) (concat (patterns song))
  where mkChunk (ps,_) line = (ps'',chunk)
          where ps' = foldl' processNote ps $ zip [0..] line
                (chunk,ps'') = mixChunk ps'
                processNote ps (i,Note ptc ins eff) = ps { psTempo = tempo'
                                                         , psBPM = bpm'
                                                         , psChannels = wrt i (psChannels ps) cs'' { csEffect = eff }
                                                         }
                  where tempo' = case eff of { [SetTempo x] -> x ; _ -> psTempo ps }
                        bpm' = case eff of { [SetBPM x] -> x ; _ -> psBPM ps }
                        cs = psChannels ps !! i
                        ins' = fromMaybe (csInstrument cs) ins
                        cs' = if ptc == 0 then cs
                               else cs { csWaveData = drop (case eff of { [SampleOffset o] -> o; _ -> 0 }) (wave ins')
                                       , csPitch = case eff of
                                         (TonePortamento _:_) -> csPitch cs
                                         _ -> ptc
                                       , csFineTune = fineTune ins'
                                       , csSubSample = 0
                                       , csSampleStep = sampleStep ptc (fineTune ins')
                                       , csVolume = volume ins'
                                       , csInstrument = ins'
                                       , csTonePortaEnd = case eff of
                                         (TonePortamento _:_) -> ptc
                                         _ -> csTonePortaEnd cs
                                       }
                        cs'' = case eff of
                          [SetVolume v] -> cs' { csVolume = v }
                          [FinePortamento (Porta p)] -> (addPitch cs p) { csLastFinePorta = abs p }
                          [FinePortamento LastUp] -> addPitch cs (-csLastFinePorta cs')
                          [FinePortamento LastDown] -> addPitch cs (csLastFinePorta cs')
                          [FineVolumeSlide x] -> let slide = fromMaybe (csLastFineSlide cs) x in 
                            cs { csVolume = max 0 $ min 1 $ csVolume cs + slide
                               , csLastFineSlide = slide
                               }
                          _ -> cs'
                                              
        wrt i xs e = let (pre,_:post) = splitAt i xs in pre ++ e:post

mixChunk ps = (concatMap snd chunks, fst (last chunks))
  where chunks = {-# SCC "chunks" #-} processTicks (psTempo ps) ps True
        tickLength = round (baseFrequency*2.5) `div` psBPM ps
        chnFact = 1 / fromIntegral (length (psChannels ps))
        processTicks 0    _  _       = []
        processTicks tick ps isFirst = (ps',mixResult) : processTicks (tick-1) ps' False
          where mixResult = {-# SCC "mixResult" #-} map (\s -> [chnFact*s]) mixedChannels
                mixedChannels = {-# SCC "mixedChannels" #-} foldl' addChannel (replicate tickLength 0) (psChannels ps)
                
                addChannel mix cs = {-# SCC "addChannel" #-} if vol < 0.0001 then mix
                                    else mixChannel mix (csWaveData cs) (csSubSample cs)
                  where step = csSampleStep cs
                        vol = csVolume cs
                        mixChannel []     wd smp = []
                        mixChannel ms     [] smp = ms
                        mixChannel (m:ms) wd smp = wsmp `seq` wsmp : mixChannel ms wd' smp'
                          where wsmp = m + head wd*vol
                                (wd',smp') = adv wd (smp+step) -- because properFraction is too slow
                                adv [] s = ([],s)
                                adv ws s | s >= 1    = adv (tail ws) (s-1)
                                         | otherwise = (ws,s)
                
                ps' = ps { psChannels = cs'' }
                cs' = map advChannel (psChannels ps)
                advChannel cs = {-# SCC "advChannel" #-} cs { csWaveData = drop wdstep (csWaveData cs)
                                   , csSubSample = smp'
                                   }
                  where (wdstep,smp') = properFraction (csSubSample cs+csSampleStep cs*fromIntegral tickLength)
                cs'' = if isFirst then cs' else flip map cs' $ \cs -> foldl' addEffect cs (csEffect cs)
                addEffect cs eff = {-# SCC "addEffect" #-} case eff of
                  Arpeggio n1 n2 ->
                    cs { csSampleStep = sampleStep (csPitch cs) (csFineTune cs) * ([1,n1,n2] !! ((psTempo ps-tick) `mod` 3))
                       }
                  Portamento (Porta p) -> let pitch = clampPitch (csPitch cs + p) in
                    cs { csPitch = pitch
                       , csSampleStep = sampleStep pitch (csFineTune cs)
                       , csLastPortaDown = if p > 0 then p else csLastPortaDown cs
                       , csLastPortaUp = if p < 0 then p else csLastPortaUp cs }
                  Portamento LastUp -> addPitch cs (csLastPortaUp cs)
                  Portamento LastDown -> addPitch cs (csLastPortaDown cs)
                  TonePortamento (Just p) -> targetPitch cs { csLastTonePorta = p }
                  TonePortamento Nothing -> targetPitch cs
                  VolumeSlide x -> let slide = fromMaybe (csLastVolumeSlide cs) x in 
                    cs { csVolume = max 0 $ min 1 $ csVolume cs + slide
                       , csLastVolumeSlide = slide
                       }
                  _ -> cs
                
addPitch cs p = let pitch = clampPitch (csPitch cs + p) in
  cs { csPitch = pitch
     , csSampleStep = sampleStep pitch (csFineTune cs)
     }

targetPitch cs = let pitch = if csPitch cs > csTonePortaEnd cs
                             then max (csTonePortaEnd cs) (csPitch cs-csLastTonePorta cs) 
                             else min (csTonePortaEnd cs) (csPitch cs+csLastTonePorta cs) 
                 in
  cs { csPitch = pitch
     , csSampleStep = sampleStep pitch (csFineTune cs)
     }

sampleStep p ft = basePeriod / (fromIntegral p * baseFrequency) * ft

clampPitch = min 1712 . max 57
