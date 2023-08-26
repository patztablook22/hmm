module Main where

import Test.HUnit
import Hmm


dostuff = do
    let path = "/home/patz/Downloads/Never-Gonna-Give-You-Up-1.mid"
    midi <- readMidi path
    print $! Midi (header midi) [annotateChords rootedChord (tracks midi !! 1)]

test1 :: Test
test1 = TestCase (dostuff)

tests :: Test
tests = TestList [TestLabel "Test1" test1]

main :: IO Counts
main = runTestTT tests
