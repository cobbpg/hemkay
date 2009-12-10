module Music where

import Data.Maybe
import Text.Printf

data Song = Song
            { title :: String
            , instruments :: [Instrument]
            , patterns :: [Pattern]
            }

instance Show Song where
  show (Song t smps pats) = unlines [line |
                                     block <- [["Title: " ++ t,""],
                                               map show smps,
                                               "":map showPattern pats
                                              ],
                                     line <- block]
                             

data Instrument = Instrument
                  { ident :: Int
                  , name :: String
                  , wave :: WaveData
                  , volume :: Float
                  , fineTune :: Float
                  }

type WaveData = [Float]

instance Eq Instrument where
  i1 == i2 = ident i1 == ident i2

instance Show Instrument where
  show (Instrument _ n dat vol ft) = "Instrument: " ++ show n ++
                                     ", samples: " ++ show (take 5 dat) ++
                                     ", volume: " ++ show vol ++
                                     ", finetune: " ++ show ft

emptyInstrument = Instrument
                  { ident = 0
                  , name = ""
                  , wave = []
                  , volume = 0
                  , fineTune = 1
                  }

type Pattern = [[Note]]

showPattern pat = unlines ["| " ++ concatMap show line | line <- pat]

data Note = Note 
            { period :: Int
            , instrument :: Maybe Instrument
            , effect :: [Effect]
            }

instance Show Note where
  show (Note p i e) = printf "%s %02d %s | " (periodName p) (maybe 0 ident i) (show e)

periodName p = head $ [str ++ show oct |
                       (oct,pers) <- zip [0..] periodTable,
                       (per,str) <- zip pers noteNames,
                       per == p] ++ ["---"]

data Waveform = SineWave | SawtoothWave | SquareWave deriving Eq

instance Show Waveform where
  show SineWave = "sin"
  show SawtoothWave = "swt"
  show SquareWave = "sqr"

data PortaParam = LastUp | LastDown | Porta Int deriving Show

data Effect = Arpeggio Float Float            -- test!
            | Portamento PortaParam           -- ok
            | TonePortamento (Maybe Int)      -- test!
            | Vibrato (Maybe Int) (Maybe Int) -- test!
            | Tremolo (Maybe Int) (Maybe Int) --
            | FinePanning Int                 --
            | SampleOffset Int                -- ok
            | VolumeSlide (Maybe Float)       -- ok
            | OrderJump Int                   --
            | SetVolume Float                 -- ok
            | PatternBreak Int                -- ok
--            | SetFilter Int                 --
            | FinePortamento PortaParam       -- test!
--            | GlissandoControl Int          --
            | SetVibratoWaveform Waveform     -- test!
            | FineTuneControl Float           -- ok
            | PatternLoop (Maybe Int)         --
            | SetTremoloWaveform Waveform     --
            | GravisPanning Int               --
            | RetrigNote Int                  -- ok
            | FineVolumeSlide (Maybe Float)   -- ok
            | NoteCut Int                     -- test!
            | NoteDelay Int                   --
            | PatternDelay Int                -- test!
--            | FunkRepeat                    --
            | SetTempo Int                    -- ok
            | SetBPM Int                      -- ok
              
instance Show Effect where
  show (Arpeggio a1 a2) = printf "arp %x %x" (unhalf a1) (unhalf a2)
    where unhalf x = round (log x / log 2 * 12) :: Int
  show (Portamento LastUp) = "por ^^^"
  show (Portamento LastDown) = "por vvv"
  show (Portamento (Porta p)) = printf "por %3d" p
  show (TonePortamento Nothing) = "ton ..."
  show (TonePortamento (Just p)) = printf "ton %3d" p
  show (Vibrato amp spd) = printf "vib %x %x" (fromMaybe 0 amp) (fromMaybe 0 spd)
  show (Tremolo amp spd) = printf "trm %x %x" (fromMaybe 0 amp) (fromMaybe 0 spd)
  show (FinePanning p) = printf "<=> %3d" p
  show (SampleOffset o) = printf "ofs $%2x" (o `div` 256)
  show (VolumeSlide Nothing) = "vsl ..."
  show (VolumeSlide (Just s)) = printf "vsl %3d" (round (s*99) :: Int)
  show (OrderJump o) = printf "ord %3d" o
  show (SetVolume v) = printf "vol %3d" (round (v*99) :: Int)
  show (PatternBreak b) = printf "brk %3d" b
  show (FinePortamento LastUp) = "por!^^^"
  show (FinePortamento LastDown) = "por!vvv"
  show (FinePortamento (Porta p)) = printf "por!%3d" p
  show (SetVibratoWaveform w) = "vib " ++ show w
  show (FineTuneControl ft) = "fin    "
  show (PatternLoop Nothing) = "lop beg"
  show (PatternLoop (Just c)) = printf "lop %3d" c
  show (SetTremoloWaveform w) = "trm " ++ show w
  show (GravisPanning p) = printf "<=> %3d" (p*8)
  show (RetrigNote r) = printf "ret %3d" r
  show (FineVolumeSlide Nothing) = "vsl!..."
  show (FineVolumeSlide (Just s)) = printf "vsl!%3d" (round (s*99) :: Int)
  show (NoteCut c) = printf "cut %3d" c
  show (NoteDelay d) = printf "ndl %3d" d
  show (PatternDelay d) = printf "pdl %3d" d
  show (SetTempo t) = printf "tmp %3d" t
  show (SetBPM b) = printf "bpm %3d" b
  
  showList [] = showString "       "
  showList [eff] = shows eff
  showList [TonePortamento _, VolumeSlide Nothing] = showString "tvs ---"
  showList [TonePortamento _, VolumeSlide (Just s)] = showString (printf "tvs %3d" (round (s*99) :: Int))
  showList [Vibrato _ _, VolumeSlide Nothing] = showString "vvs ---"
  showList [Vibrato _ _, VolumeSlide (Just s)] = showString (printf "vvs %3d" (round (s*99) :: Int))
  showList _ = showString "???????"


periodTable = [[1712, 1616, 1525, 1440, 1375, 1281, 1209, 1141, 1077, 1017,  961,  907],
               [ 856,  808,  762,  720,  678,  640,  604,  570,  538,  508,  480,  453],
               [ 428,  404,  381,  360,  339,  320,  302,  285,  269,  254,  240,  226],
               [ 214,  202,  190,  180,  170,  160,  151,  143,  135,  127,  120,  113],
               [ 107,  101,   95,   90,   85,   80,   76,   71,   67,   64,   60,   57]]

noteNames = ["C-", "C#", "D-", "D#", "E-", "F-", "F#", "G-", "G#", "A-", "A#", "B-"]

waveData :: [(Waveform, [Int])]
waveData = [(SineWave, cycle
             [ -1,  1,  3,  4,  5,  7,  8,  9, 10, 11, 12, 13, 14, 14, 15, 15
             , 15, 15, 15, 14, 14, 13, 12, 11, 10,  9,  8,  7,  5,  4,  3,  1
             , -1, -2, -4, -5, -6, -8, -9,-10,-11,-12,-13,-14,-15,-15,-16,-16
             ,-16,-16,-16,-15,-15,-14,-13,-12,-11,-10, -9, -8, -6, -5, -4, -2])
           ,(SawtoothWave, cycle           
             [ 15, 15, 14, 14, 13, 13, 12, 12, 11, 11, 10, 10,  9,  9,  8,  8
             ,  7,  7,  6,  6,  5,  5,  4,  4,  3,  3,  2,  2,  1,  1,  0,  0
             , -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -6, -6, -7, -7, -8, -8
             , -9, -9,-10,-10,-11,-11,-12,-12,-13,-13,-14,-14,-15,-15,-16,-16])
           ,(SquareWave, cycle
             [ 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15
             , 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15
             ,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16
             ,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16])
           ]
