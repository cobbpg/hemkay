module Player where

import Control.Monad
import Control.Monad.Fix
import Data.List
--import Prelude hiding (replicate,head,concat,map,(++),take,span,null,drop,last,
--                       concatMap,zipWith,(!!),tail,scanl,zip,splitAt,length)
--import Data.List.Stream
import Data.Maybe
import Sound.PortAudio
import Text.Printf

import Music

baseFrequency = 3546894.6

sampleFrequency = 44100

bufferLength = 0x1000

playModule song = withDefaultStream 0 1 paFloat32 (realToFrac sampleFrequency) bufferLength $ \stream _ -> do
  --flip fix (concatMap snd (mixSong song)) $ \consume waveData -> do
  --  let (chunk,rest) = splitAt bufferLength waveData
  --  writeStream stream chunk bufferLength
  --  when (not (null rest)) $ consume rest

  flip fix (0,mixSong song) $ \playback (tick,chunks) -> unless (null chunks) $ do
    let (state,chunk):rest = chunks
    forM_ chunk $ \x -> writeStream stream [x] 1
    if tick <= 0 then print state >> playback (psTempo state-1,rest)
      else playback (tick-1,rest)

  --forM_ (mixSong song) $ \(state,chunk) -> print state >> forM_ chunk (\x -> writeStream stream [x] 1)

data PlayState = PS
                 { psTempo :: Int
                 , psBPM :: Int
                 , psChannels :: [ChannelState]
                 }

instance Show PlayState where
  show (PS t b cs) = printf "%d/%d %s" t b (show cs)

data ChannelState = CS
                    { csWaveData :: WaveData
                    , csPeriod :: Int
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
                    , csVibratoWave :: [Float]
                    }

instance Show ChannelState where
  show cs = printf "<%s %02d %02d %s>" (periodName (csPeriod cs)) (ident (csInstrument cs))
            (round (csVolume cs*99) :: Int) (show (csEffect cs))

startState numChn = PS { psTempo = 6
                       , psBPM = 125
                       , psChannels = replicate numChn chn
                       }
  where chn = CS { csWaveData = []
                 , csPeriod = 0
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
  Panning         : Array[1..8] Of ShortInt;
  Surround        : Array[1..8] Of Boolean;
  LastTremolo     : Array[1..8] Of Byte;
  TremoloTable    : WavePtr;
  TremoloCounter  : Array[1..8] Of Byte;
-}

mixSong song = (map.fmap.map) (\s -> [chnFact*s]) . map mixChunk . expandSong $ song
  where chnFact = 1 / fromIntegral (numChannels song)

mixChunk state = (state,mixedChannels)
  where mixedChannels = foldl' addChannel emptyChannel (psChannels state)
        emptyChannel = replicate (tickLength (psBPM state)) 0
        addChannel mix cs = if vol < 0.0001 then mix else mixChannel mix (csWaveData cs) (csSubSample cs)
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

expandSong song = expandTicks (numChannels song) . flattenSong $ song

flattenSong = concat . map handleLoops . map handleDelays . handleBreaks 0 . patterns
  where handleBreaks _   []         = []
        handleBreaks row (pat:pats) = (pat' ++ take 1 rest) : handleBreaks row' pats
          where (pat',rest) = span (null . getBreaks) (drop row pat)
                row' = maybe 0 (last . getBreaks) (listToMaybe (take 1 rest))
                getBreaks line = [b | [PatternBreak b] <- map effect line]

        handleDelays = concatMap (\l -> replicate (delayCount l) l)
          where delayCount line = last (1:[d | [PatternDelay d] <- map effect line])
        
        handleLoops pat = pat' ++ if null rest' then loop else rest''
          where (pat',rest) = span noLoopStart pat
                (loop,rest') = span (isNothing . getLoopEnd) rest
                loopLast:loopRest = rest'
                rest'' = case getLoopEnd loopLast of
                  Just cnt -> concat (replicate (cnt+1) (loop ++ [loopLast])) ++ handleLoops loopRest
                noLoopStart line = null [() | [PatternLoop Nothing] <- map effect line]
                getLoopEnd line = listToMaybe [cnt | [PatternLoop (Just cnt)] <- map effect line]

expandTicks channelCount = unfoldr expandLine . (,,) 0 (startState channelCount)
  where expandLine (0,_,[]) = Nothing
        expandLine (0,PS tempo bpm channels,(line:lines)) = Just (state,(tick',state',lines))
          where tempo' = last (tempo:[x | [SetTempo x] <- map effect line])
                bpm' = last (bpm:[x | [SetBPM x] <- map effect line])
                tick' = if tempo > 1 then 1 else 0
                state = PS tempo' bpm' $ zipWith (processNote (tickLength bpm')) line channels
                state' = advanceSamples state
        expandLine (tick,PS tempo bpm channels,lines) = Just (state,(tick',state',lines))
          where tick' = if tick < tempo-1 then tick+1 else 0
                state = PS tempo bpm $ map (processChannel (tickLength bpm) tick) channels
                state' = advanceSamples state

        advanceSamples state = state { psChannels = map advanceSample (psChannels state) }
          where tickLen = tickLength (psBPM state)
                advanceSample cs = cs { csWaveData = drop wdstep (csWaveData cs)
                                      , csSubSample = smp'
                                      }
                  where (wdstep,smp') = properFraction (csSubSample cs+csSampleStep cs*fromIntegral tickLen)

tickLength bpm = round (sampleFrequency*2.5) `div` bpm

processNote tickLen (Note per ins eff) cs = (addPeriod cs'' 0) { csEffect = eff }
  where ins' = fromMaybe (csInstrument cs) ins
        insStays = isNothing ins || ins == Just (csInstrument cs)

        -- Handling the new note
        cs' = if per == 0
              then cs { csInstrument = ins'
                      , csVolume = if isJust ins then volume ins' else csVolume cs
                      , csFineTune = fineTune ins'
                      , csWaveData = if insStays then csWaveData cs else wave ins'
                      }
              else cs { csInstrument = ins' 
                      , csVolume = volume ins'
                      , csFineTune = fineTune ins'
                      , csWaveData = case eff of
                           [SampleOffset o] -> drop o (wave ins')
                           _ -> wave ins'
                      , csPeriod = per
                      }

        -- Setting up the new effect
        cs'' = case eff of
          (TonePortamento _:_) ->
            cs' { csWaveData = if insStays then csWaveData cs else wave ins'
                , csPeriod = csPeriod cs
                , csTonePortaEnd = if per == 0 then csTonePortaEnd cs else per
                }
          (Vibrato spd amp:_) ->
            cs' { csVibratoSpeed = fromMaybe (csVibratoSpeed cs') spd
                , csVibratoAmp = maybe (csVibratoAmp cs') ((*2).fromIntegral) amp
                }
          [SetVolume v] -> cs' { csVolume = v }
          [FinePortamento (Porta p)] -> (addPeriod cs' p) { csFinePorta = abs p }
          [FinePortamento LastUp] -> addPeriod cs' (-csFinePorta cs')
          [FinePortamento LastDown] -> addPeriod cs' (csFinePorta cs')
          [SetVibratoWaveform wf] -> cs' { csVibratoWave = (snd.fromJust) (find ((==wf).fst) waveData) }
          [FineVolumeSlide x] -> let slide = fromMaybe (csFineVolumeSlide cs') x in 
            cs' { csVolume = max 0 $ min 1 $ csVolume cs' + slide
                , csFineVolumeSlide = slide
                }
          [FineTuneControl ft] -> addPeriod cs' { csFineTune = ft } 0
          _ -> cs'

processChannel tickLen tick cs = foldl' addEffect cs (csEffect cs)
  where addEffect cs eff = case eff of
          Arpeggio n1 n2 ->
            cs { csSampleStep = sampleStep (csPeriod cs) (csFineTune cs) * ([1,n1,n2] !! (tick `mod` 3)) }
          Portamento (Porta p) -> (addPeriod cs p) { csPortaDown = if p > 0 then p else csPortaDown cs
                                                  , csPortaUp = if p < 0 then p else csPortaUp cs
                                                  }
          Portamento LastUp -> addPeriod cs (csPortaUp cs)
          Portamento LastDown -> addPeriod cs (csPortaDown cs)
          TonePortamento (Just p) -> targetPeriod cs { csTonePortaSpeed = p }
          TonePortamento Nothing -> targetPeriod cs
          Vibrato _ _ -> let period = csPeriod cs + round (head (csVibratoWave cs) * csVibratoAmp cs) in
                         cs { csSampleStep = sampleStep period (csFineTune cs)
                            , csVibratoWave = drop (csVibratoSpeed cs) (csVibratoWave cs)
                            }
          VolumeSlide x -> let slide = fromMaybe (csVolumeSlide cs) x in 
            cs { csVolume = max 0 $ min 1 $ csVolume cs + slide
               , csVolumeSlide = slide
               }
          RetrigNote r -> if tick `mod` r == 0 then cs { csWaveData = wave (csInstrument cs) } else cs
          NoteCut c -> if tick == c then cs { csWaveData = [] } else cs
          _ -> cs
                
addPeriod cs p = cs { csPeriod = period
                   , csSampleStep = sampleStep period (csFineTune cs)
                   }
  where period = clampPeriod (csPeriod cs + p)

targetPeriod cs = cs { csPeriod = period
                    , csSampleStep = sampleStep period (csFineTune cs)
                    } 
  where period = if csPeriod cs > csTonePortaEnd cs
                then max (csTonePortaEnd cs) (csPeriod cs-csTonePortaSpeed cs) 
                else min (csTonePortaEnd cs) (csPeriod cs+csTonePortaSpeed cs) 

sampleStep p ft = baseFrequency / (fromIntegral p * sampleFrequency) * ft

sampleStep' p ft = baseFrequency / (p * sampleFrequency) * ft

clampPeriod = min 1712 . max 57
