module Main where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable
import System.Process
import Text.Printf
import Data.List

type Pulse = Float
type Seconds = Float
type Samples = Float
type Hz = Float
type Semitones = Float

outputFilePath :: FilePath
outputFilePath = "output.bin"

volume :: Float
volume = 0.2

sampleRate :: Samples
sampleRate = 48000.0

pitchStandard :: Hz
pitchStandard = 440.0


-- NOTE: the formula is taken from https://pages.mtu.edu/~suits/NoteFreqCalcs.html
f :: Semitones -> Hz
f n = pitchStandard * (2 ** (1.0 / 12.0)) ** n


generaArmonico :: Float -> Hz
generaArmonico n = if (n >= 2.0) then generaArmonico (n / 2.0) else n

freq :: Hz -> Seconds -> [Pulse]
freq hz duration =
  map (* volume) output
  where
    step = (hz * 2 * pi) / sampleRate

    output :: [Pulse]
    output = map sin $ map (* step) [0.0 .. sampleRate * duration]

arpeggio :: [Pulse]
arpeggio = concat [ freq pitchStandard 2.0
                  , freq (pitchStandard * (5.0 / 4.0)) 2.0
				  , freq (pitchStandard * 1.5) 2.0
				  ]


armonici :: [Pulse]
armonici = concat $ map (\f -> freq f 2.0) $ map (* pitchStandard) $ sort $ Data.List.nub $ map generaArmonico [1..20]

save :: FilePath -> IO ()
save filePath = B.writeFile filePath $ B.toLazyByteString $ fold $ map B.floatLE armonici

play :: IO ()
play = do
  save outputFilePath
  _ <- runCommand $ printf "ffplay -autoexit -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()

main :: IO ()
main = play
