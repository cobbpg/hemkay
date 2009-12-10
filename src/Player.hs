module Player where

import Control.Monad
import Control.Monad.Fix
import Data.List
import Data.Maybe
import Sound.PortAudio
import Text.Printf

import Music

basePeriod = 3546894.6

baseFrequency = 44100

bufferLength = 0x1000

playModule song = withDefaultStream 0 1 paFloat32 (realToFrac baseFrequency) bufferLength $ \stream _ -> do
  --flip fix (concatMap snd (mixSong song)) $ \consume waveData -> do
  --  let (chunk,rest) = splitAt bufferLength waveData
  --  writeStream stream chunk bufferLength
  --  when (not (null rest)) $ consume rest
  forM_ (mixSong song) $ \(state,chunk) -> print state >> forM_ chunk (\x -> writeStream stream [x] 1)
  --writeStream stream chunk bufferLength

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
--                    , csNextInstrument :: Instrument
                    , csEffect :: [Effect]
                    , csPortaDown :: Int
                    , csPortaUp :: Int
                    , csFinePorta :: Int
                    , csTonePortaEnd :: Int
                    , csTonePortaSpeed :: Int
                    , csVolumeSlide :: Float
                    , csFineVolumeSlide :: Float
                    , csVibratoSpeed :: Int
                    , csVibratoAmp :: Float
                    , csVibratoWave :: [Int]
                    }

instance Show ChannelState where
  show cs = printf "<%s %02d %02d %s>" (periodName (csPitch cs)) (ident (csInstrument cs))
            (round (csVolume cs*99) :: Int) (show (csEffect cs))

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
                 , csInstrument = emptyInstrument
--                 , csNextInstrument = emptyInstrument
                 , csEffect = []
                 , csPortaDown = 0
                 , csPortaUp = 0
                 , csFinePorta = 0
                 , csTonePortaEnd = 0
                 , csTonePortaSpeed = 0
                 , csVolumeSlide = 0
                 , csFineVolumeSlide = 0
                 , csVibratoSpeed = 0
                 , csVibratoAmp = 0
                 , csVibratoWave = snd (head waveData)
                 }

{-
  State vars still missing:

  DelayChannel    : Byte;
  PatternLoopCnt  : Byte;
  PatternLoopRow  : Byte;
  PatternLoop     : Boolean;
  Panning         : Array[1..8] Of ShortInt;
  Surround        : Array[1..8] Of Boolean;
  LastTremolo     : Array[1..8] Of Byte;
  TremoloTable    : WavePtr;
  TremoloCounter  : Array[1..8] Of Byte;
  NewOrder        : Byte;
-}

mixSong song = tail $ scanl mkChunk (startState song,[]) (flattenRows (patterns song))
  where flattenRows = concat . handleDelays . handleBreaks 0
        handleBreaks _ [] = []
        handleBreaks row (pat:pats) = (pat' ++ take 1 rest) : handleBreaks row' pats
          where (pat',rest) = span (all (isNothing . getBreak)) (drop row pat)
                row' = head ([b | Just b <- map getBreak (last pat')] ++ [0])
                getBreak note = case effect note of
                  [PatternBreak b] -> Just b
                  _ -> Nothing
        handleDelays pat = map (concatMap (\r -> replicate (delayCount r) r)) pat
          where delayCount r = head ([d+1 | Just d <- map getDelay r] ++ [1])
                getDelay note = case effect note of
                  [PatternDelay d] -> Just d
                  _ -> Nothing
                
        mkChunk (ps,_) line = (ps'',chunk)
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
                        
                        -- This part really needs to be cleaned up...
                        cs' = if ptc == 0 then cs { csInstrument = ins'
                                                  , csVolume = if isJust ins then volume ins' else csVolume cs
                                                  , csWaveData = if isNothing ins || ins == Just (csInstrument cs)
                                                                 then csWaveData cs else wave ins'
                                                  }
                               else cs { csWaveData = case eff of
                                         (TonePortamento _:_) -> if ins == Just (csInstrument cs)
                                                                 then csWaveData cs else wave ins'
                                         [SampleOffset o] -> drop o (wave ins')
                                         _ -> wave ins'
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
                          (Vibrato spd amp:_) ->
                            cs' { csVibratoSpeed = fromMaybe (csVibratoSpeed cs') spd
                                , csVibratoAmp = maybe (csVibratoAmp cs') ((/8).fromIntegral) amp
                                }
                          [SetVolume v] -> cs' { csVolume = v }
                          [FinePortamento (Porta p)] -> (addPitch cs p) { csFinePorta = abs p }
                          [FinePortamento LastUp] -> addPitch cs (-csFinePorta cs')
                          [FinePortamento LastDown] -> addPitch cs (csFinePorta cs')
                          [SetVibratoWaveform wf] -> cs { csVibratoWave = (snd.fromJust) (find ((==wf).fst) waveData) }
                          [FineVolumeSlide x] -> let slide = fromMaybe (csFineVolumeSlide cs) x in 
                            cs { csVolume = max 0 $ min 1 $ csVolume cs + slide
                               , csFineVolumeSlide = slide
                               }
                          [FineTuneControl ft] -> addPitch cs { csFineTune = ft } 0
                          _ -> cs'
                                              
        wrt i xs e = let (pre,_:post) = splitAt i xs in pre ++ e:post

mixChunk ps = (concatMap snd chunks, fst (last chunks))
  where chunks = processTicks (psTempo ps) ps True
        tickLength = round (baseFrequency*2.5) `div` psBPM ps
        chnFact = 1 / fromIntegral (length (psChannels ps))
        processTicks 0    _  _       = []
        processTicks tick ps isFirst = (ps',mixResult) : processTicks (tick-1) ps' False
          where mixResult = map (\s -> [chnFact*s]) mixedChannels
                mixedChannels = foldl' addChannel (replicate tickLength 0) (psChannels ps)
                curTick = psTempo ps-tick
                
                addChannel mix cs = if vol < 0.0001 then mix
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
                advChannel cs = cs { csWaveData = drop wdstep (csWaveData cs)
                                   , csSubSample = smp'
                                   }
                  where (wdstep,smp') = properFraction (csSubSample cs+csSampleStep cs*fromIntegral tickLength)
                cs'' = if isFirst then cs' else flip map cs' $ \cs -> foldl' addEffect cs (csEffect cs)                
                addEffect cs eff = case eff of
                  Arpeggio n1 n2 ->
                    cs { csSampleStep = sampleStep (csPitch cs) (csFineTune cs) * ([1,n1,n2] !! (curTick `mod` 3))
                       }
                  Portamento (Porta p) -> let pitch = clampPitch (csPitch cs + p) in
                    cs { csPitch = pitch
                       , csSampleStep = sampleStep pitch (csFineTune cs)
                       , csPortaDown = if p > 0 then p else csPortaDown cs
                       , csPortaUp = if p < 0 then p else csPortaUp cs }
                  Portamento LastUp -> addPitch cs (csPortaUp cs)
                  Portamento LastDown -> addPitch cs (csPortaDown cs)
                  TonePortamento (Just p) -> targetPitch cs { csTonePortaSpeed = p }
                  TonePortamento Nothing -> targetPitch cs
                  Vibrato _ _ -> let pitch = fromIntegral (csPitch cs) +
                                             fromIntegral (head (csVibratoWave cs)) * csVibratoAmp cs in
                    cs { csSampleStep = sampleStep' pitch (csFineTune cs)
                       , csVibratoWave = drop (csVibratoSpeed cs) (csVibratoWave cs)
                       }
                  VolumeSlide x -> let slide = fromMaybe (csVolumeSlide cs) x in 
                    cs { csVolume = max 0 $ min 1 $ csVolume cs + slide
                       , csVolumeSlide = slide
                       }
                  RetrigNote r -> if curTick `mod` r == 0 then cs { csWaveData = wave (csInstrument cs) } else cs
                  NoteCut c -> if curTick == c then cs { csWaveData = [] } else cs
                  _ -> cs
                
addPitch cs p = let pitch = clampPitch (csPitch cs + p) in
  cs { csPitch = pitch
     , csSampleStep = sampleStep pitch (csFineTune cs)
     }

targetPitch cs = let pitch = if csPitch cs > csTonePortaEnd cs
                             then max (csTonePortaEnd cs) (csPitch cs-csTonePortaSpeed cs) 
                             else min (csTonePortaEnd cs) (csPitch cs+csTonePortaSpeed cs) 
                 in
  cs { csPitch = pitch
     , csSampleStep = sampleStep pitch (csFineTune cs)
     }

sampleStep p ft = basePeriod / (fromIntegral p * baseFrequency) * ft

sampleStep' p ft = basePeriod / (p * baseFrequency) * ft

clampPitch = min 1712 . max 57
