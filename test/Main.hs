module Main where

import Test.HUnit
import Hmm


dostuff = do
    let (semitonesStr : input : output : _) = ["-1", "never-gonna-give-you-up.mid", "asdf.mid"]
    let semitones = read semitonesStr :: Int
    midi <- readMidi input
    let midi' = Midi {header = header midi,
                      tracks = map (transposeTrack semitones) (tracks midi)}
    writeMidi output midi'

test1 :: Test
test1 = TestCase (dostuff)

tests :: Test
tests = TestList [TestLabel "Test1" test1]

main :: IO Counts
main = runTestTT tests
