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
  show (Note p i e) = note ++ " " ++ ins ++ " " ++ eff ++ " | "
    where note = periodName p
          ins = maybe "<none>" name i
          eff = "---" -- Yes, it could be more informative...

periodName p = head $ [str ++ show oct |
                       (oct,pers) <- zip [0..] periodTable,
                       (per,str) <- zip pers noteNames,
                       per == p] ++ ["---"]

data Waveform = SineWave | SawtoothWave | SquareWave deriving (Show, Eq)

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
            deriving Show
              
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
