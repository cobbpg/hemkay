{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

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

baseFrequency :: Float
baseFrequency = 3546894.6

sampleFrequency :: Float
sampleFrequency = 44100

bufferLength :: Int
bufferLength = 0x1000

playModule :: Song -> IO (Either String ())
playModule song = withDefaultStream 0 2 paFloat32 (realToFrac sampleFrequency) bufferLength $ \stream _ -> do
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
  show (PS t b cs) = printf "%2d/%3d %s" t b (show cs)

data ChannelState = CS
                    { csWaveData :: WaveData
                    , csPeriod :: Int
                    , csFineTune :: Float
                    , csSubSample :: Float
                    , csSampleStep :: Float
                    , csVolume :: Float
                    , csInstrument :: Instrument
                    , csEffect :: [Effect]
                    , csPanning :: Float
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
                    , csTremoloSpeed :: Int
                    , csTremoloAmp :: Float
                    , csTremoloWave :: [Float]
                    , csTremoloDiff :: Float
                    , csDelayedPeriod :: Int
                    , csDelayedInstrument :: Instrument
                    }

instance Show ChannelState where
  show cs = printf "<%s %02d %02d %s>" (periodName (csPeriod cs)) (ident (csInstrument cs))
            (round (csVolume cs*99) :: Int) (show (csEffect cs))

startState :: Int -> PlayState
startState numChn = PS { psTempo = 6
                       , psBPM = 125
                       , psChannels = map chn [0..numChn]
                       }
  where chn n = CS { csWaveData = []
                   , csPeriod = 0
                   , csFineTune = 1
                   , csSubSample = 0
                   , csSampleStep = 0
                   , csVolume = 1
                   , csInstrument = emptyInstrument
                   , csEffect = []
                   , csPanning = if (n-1) `mod` 4 < 2 then 0.8 else 0.2
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
                   , csTremoloSpeed = 0
                   , csTremoloAmp = 0
                   , csTremoloWave = snd (head waveData)
                   , csTremoloDiff = 0
                   , csDelayedPeriod = 0
                   , csDelayedInstrument = emptyInstrument
                   }

mixSong :: Song -> [(PlayState, [[Float]])]
mixSong song = (map.fmap.map) (\(sl,sr) -> [chnFact*sl,chnFact*sr]) . map mixChunk . expandSong $ song
  where chnFact = 1 / fromIntegral (numChannels song)

mixChunk :: PlayState -> (PlayState, [(Float, Float)])
mixChunk state = (state,mixedChannels)
  where mixedChannels = foldl' addChannel emptyChannel (psChannels state)
        emptyChannel = replicate (tickLength (psBPM state)) (0,0)
        addChannel mix cs = if vol < 0.0001 then mix else mixChannel mix (csWaveData cs) (csSubSample cs)
          where step = csSampleStep cs
                vol = clampVolume (csVolume cs + csTremoloDiff cs)
                pan = csPanning cs
                mixChannel []           _  _   = []
                mixChannel ms           [] _   = ms
                mixChannel ((ml,mr):ms) wd smp = mlr' `seq` mlr' : mixChannel ms wd' smp'
                  where wsmp = head wd*vol
                        ml' = ml+wsmp*(1-pan)
                        mr' = mr+wsmp*pan
                        mlr' = ml' `seq` mr' `seq` (ml',mr')
                        (wd',smp') = adv wd (smp+step) -- because properFraction is too slow
                        adv [] s = ([],s)
                        adv ws s | s >= 1    = adv (tail ws) (s-1)
                                 | otherwise = (ws,s)

expandSong :: Song -> [PlayState]
expandSong song = expandTicks (numChannels song) . flattenSong $ song

flattenSong :: Song -> [[Note]]
flattenSong = concat . map handleLoops . map handleDelays . handleBreaks 0 . patterns
  where handleBreaks _   []         = []
        handleBreaks row (pat:pats) = (pat' ++ take 1 rest) : handleBreaks row' pats
          where (pat',rest) = span (null . getBreaks) (drop row pat)
                row' = maybe 0 (last . getBreaks) (listToMaybe (take 1 rest))
                getBreaks row = [b | [PatternBreak b] <- map effect row]

        handleDelays = concatMap (\l -> replicate (delayCount l) l)
          where delayCount row = last (1:[d | [PatternDelay d] <- map effect row])
        
        handleLoops pat = pat' ++ if null rest' then loop else rest''
          where (pat',rest) = span noLoopStart pat
                (loop,rest') = span (isNothing . getLoopEnd) rest
                loopLast:loopRest = rest'
                rest'' = case getLoopEnd loopLast of
                  Just cnt -> concat (replicate (cnt+1) (loop ++ [loopLast])) ++ handleLoops loopRest
                  Nothing -> loop
                noLoopStart row = null [() | [PatternLoop Nothing] <- map effect row]
                getLoopEnd row = listToMaybe [cnt | [PatternLoop (Just cnt)] <- map effect row]

expandTicks :: Int -> [[Note]] -> [PlayState]
expandTicks channelCount = unfoldr expandRow . (,,) 0 (startState channelCount)
  where expandRow (0,_,[]) = Nothing
        expandRow (0,PS tempo bpm channels,(row:rows)) = Just (state,(tick',state',rows))
          where tempo' = last (tempo:[x | [SetTempo x] <- map effect row])
                bpm' = last (bpm:[x | [SetBPM x] <- map effect row])
                tick' = if tempo > 1 then 1 else 0
                state = PS tempo' bpm' $ zipWith processNote row channels
                state' = advanceSamples state
        expandRow (tick,PS tempo bpm channels,rows) = Just (state,(tick',state',rows))
          where tick' = if tick < tempo-1 then tick+1 else 0
                state = PS tempo bpm $ map (processChannel tick) channels
                state' = advanceSamples state

        advanceSamples state = state { psChannels = map advanceSample (psChannels state) }
          where tickLen = tickLength (psBPM state)
                advanceSample cs = cs { csWaveData = drop wdstep (csWaveData cs)
                                      , csSubSample = smp'
                                      }
                  where (wdstep,smp') = properFraction (csSubSample cs+csSampleStep cs*fromIntegral tickLen)

tickLength :: Int -> Int
tickLength bpm = round (sampleFrequency*2.5) `div` bpm

processNote :: Note -> ChannelState -> ChannelState
processNote (Note per ins eff) cs = cs'''
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
          (Tremolo spd amp:_) ->
            cs' { csTremoloSpeed = fromMaybe (csTremoloSpeed cs') spd
                , csTremoloAmp = maybe (csTremoloAmp cs') ((/64).fromIntegral) amp
                }
          [FinePanning p] -> cs' { csPanning = p }
          [SetVolume v] -> cs' { csVolume = v }
          [FinePortamento (Porta p)] -> (addPeriod cs' p) { csFinePorta = abs p }
          [FinePortamento LastUp] -> addPeriod cs' (-csFinePorta cs')
          [FinePortamento LastDown] -> addPeriod cs' (csFinePorta cs')
          [SetVibratoWaveform wf] -> cs' { csVibratoWave = (snd.fromJust) (find ((==wf).fst) waveData) }
          [SetTremoloWaveform wf] -> cs' { csTremoloWave = (snd.fromJust) (find ((==wf).fst) waveData) }
          [FineVolumeSlide x] -> let slide = fromMaybe (csFineVolumeSlide cs') x in 
            cs' { csVolume = max 0 $ min 1 $ csVolume cs' + slide
                , csFineVolumeSlide = slide
                }
          [FineTuneControl ft] -> addPeriod cs' { csFineTune = ft } 0
          [NoteDelay _] -> if per == 0 then cs' else
            cs' { csInstrument = csInstrument cs
                , csVolume = csVolume cs
                , csFineTune = csFineTune cs
                , csWaveData = csWaveData cs
                , csDelayedPeriod = per
                , csDelayedInstrument = ins'
                }
          _ -> cs'
        
        -- Finalising state and handling vibrato effects  
        cs''' = handleVibs (addPeriod cs'' 0) { csEffect = eff, csTremoloDiff = 0 }
        handleVibs cs = case eff of
          (Vibrato _ _:_) -> let period = clampPeriod (csPeriod cs + round (head (csVibratoWave cs) * csVibratoAmp cs)) in
            cs { csSampleStep = sampleStep period (csFineTune cs)
               , csVibratoWave = drop (csVibratoSpeed cs) (csVibratoWave cs)
               }
          (Tremolo _ _:_) ->
            cs { csTremoloDiff = head (csTremoloWave cs) * csTremoloAmp cs
               , csTremoloWave = drop (csTremoloSpeed cs) (csTremoloWave cs)
               }
          _ -> cs

processChannel :: Int -> ChannelState -> ChannelState
processChannel tick cs = foldl' addEffect cs (csEffect cs)
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
          Vibrato _ _ -> let period = clampPeriod (csPeriod cs + round (head (csVibratoWave cs) * csVibratoAmp cs)) in
                         cs { csSampleStep = sampleStep period (csFineTune cs)
                            , csVibratoWave = drop (csVibratoSpeed cs) (csVibratoWave cs)
                            }
          Tremolo _ _ -> cs { csTremoloDiff = head (csTremoloWave cs) * csTremoloAmp cs
                            , csTremoloWave = drop (csTremoloSpeed cs) (csTremoloWave cs)
                            }
          VolumeSlide x -> let slide = fromMaybe (csVolumeSlide cs) x in 
            cs { csVolume = clampVolume (csVolume cs + slide)
               , csVolumeSlide = slide
               }
          RetrigNote r -> if tick `mod` r == 0 then cs { csWaveData = wave (csInstrument cs) } else cs
          NoteCut c -> if tick == c then cs { csVolume = 0 } else cs
          NoteDelay d -> if tick /= d then cs
                         else let ins = csDelayedInstrument cs in
                         flip addPeriod 0 cs { csInstrument = ins
                                             , csVolume = volume ins
                                             , csFineTune = fineTune ins
                                             , csWaveData = wave ins
                                             , csPeriod = csDelayedPeriod cs
                                             }
          _ -> cs
                
addPeriod :: ChannelState -> Int -> ChannelState
addPeriod cs p = cs { csPeriod = period
                    , csSampleStep = sampleStep period (csFineTune cs)
                    }
  where period = clampPeriod (csPeriod cs + p)

targetPeriod :: ChannelState -> ChannelState
targetPeriod cs = cs { csPeriod = period
                     , csSampleStep = sampleStep period (csFineTune cs)
                     } 
  where period = if csPeriod cs > csTonePortaEnd cs
                then max (csTonePortaEnd cs) (csPeriod cs-csTonePortaSpeed cs) 
                else min (csTonePortaEnd cs) (csPeriod cs+csTonePortaSpeed cs) 

sampleStep :: Int -> Float -> Float
sampleStep p ft = baseFrequency / (fromIntegral p * sampleFrequency) * ft

clampPeriod :: Int -> Int
clampPeriod = min 1712 . max 57

clampVolume :: Float -> Float
clampVolume = max 0 . min 1
