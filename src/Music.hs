module Music where

import Text.Printf

data Song = Song
            { title :: String
            , samples :: [Sample]
            , patterns :: [Pattern]
            }

instance Show Song where
  show (Song t smps pats) = unlines [line |
                                     block <- [["Title: " ++ t,""],
                                               map show smps,
                                               "":map showPattern pats
                                              ],
                                     line <- block]
                             

data Sample = Sample
              { name :: String
              , wave :: [Float]
              , volume :: Int
              , fineTune :: Int
              }

instance Show Sample where
  show (Sample n dat vol ft) = "Instrument: " ++ show n ++
                               ", samples: " ++ show (take 5 dat) ++
                               ", volume: " ++ show vol ++
                               ", finetune: " ++ show ft

type Pattern = [[Note]]

showPattern pat = unlines ["| " ++ concatMap show line | line <- pat]

data Note = Note 
            { period :: Int
            , instrument :: Int
            , effect :: [Effect]
            }

instance Show Note where
  show (Note p i e) = note ++ " " ++ ins ++ " " ++ eff ++ " | "
    where note = head $ [str ++ show oct |
                         (oct,pers) <- zip [0..] periodTable,
                         (per,str) <- zip pers noteNames,
                         per == p] ++ ["---"]
          ins = if i == 0 then "--" else printf "%2d" i
          eff = "---"

data Waveform = SineWave | SawtoothWave | SquareWave deriving Show

data PortaParam = LastUp | LastDown | Porta Int deriving Show

data Effect = Arpeggio Int Int
            | Portamento PortaParam
            | TonePortamento (Maybe Int)
            | Vibrato (Maybe Int) (Maybe Int)
            | Tremolo (Maybe Int) (Maybe Int)
            | FinePanning Int
            | SampleOffset Int
            | VolumeSlide (Maybe Int)
            | OrderJump Int
            | SetVolume Int
            | PatternBreak Int
--            | SetFilter Int
            | FinePortamento PortaParam
--            | GlissandoControl Int
            | SetVibratoWaveform Waveform
            | FineTuneControl Int
            | PatternLoop (Maybe Int)
            | SetTremoloWaveform Waveform
            | GravisPanning Int
            | RetrigNote Int
            | FineVolumeSlide (Maybe Int)
            | NoteCut Int
            | NoteDelay Int
            | PatternDelay Int
--            | FunkRepeat
            | SetTempo Int 
            | SetBPM Int
            deriving Show
              
periodTable = [[1712, 1616, 1525, 1440, 1375, 1281, 1209, 1141, 1077, 1017,  961,  907],
               [ 856,  808,  762,  720,  678,  640,  604,  570,  538,  508,  480,  453],
               [ 428,  404,  381,  360,  339,  320,  302,  285,  269,  254,  240,  226],
               [ 214,  202,  190,  180,  170,  160,  151,  143,  135,  127,  120,  113],
               [ 107,  101,   95,   90,   85,   80,   76,   71,   67,   64,   60,   57]]

noteNames = ["C-", "C#", "D-", "D#", "E-", "F-", "F#", "G-", "G#", "A-", "A#", "B-"]

waveData = [(SineWave,
             [ -1,  1,  3,  4,  5,  7,  8,  9, 10, 11, 12, 13, 14, 14, 15, 15
             , 15, 15, 15, 14, 14, 13, 12, 11, 10,  9,  8,  7,  5,  4,  3,  1
             , -1, -2, -4, -5, -6, -8, -9,-10,-11,-12,-13,-14,-15,-15,-16,-16
             ,-16,-16,-16,-15,-15,-14,-13,-12,-11,-10, -9, -8, -6, -5, -4, -2])
           ,(SawtoothWave,             
             [ 15, 15, 14, 14, 13, 13, 12, 12, 11, 11, 10, 10,  9,  9,  8,  8
             ,  7,  7,  6,  6,  5,  5,  4,  4,  3,  3,  2,  2,  1,  1,  0,  0
             , -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -6, -6, -7, -7, -8, -8
             , -9, -9,-10,-10,-11,-11,-12,-12,-13,-13,-14,-14,-15,-15,-16,-16])
           ,(SquareWave,
             [ 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15
             , 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15
             ,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16
             ,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16,-16])
           ]
