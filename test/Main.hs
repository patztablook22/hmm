module Main where

import Test.HUnit
import Hmm
import qualified Data.Set as S
import Data.Maybe

instantChange (MidiEvent t1 _) ((MidiEvent t2 _):_) = t1 == t2
instantChange _ _ = False

annotate = annotate1 S.empty
annotate1 :: S.Set Note -> MidiTrack -> MidiTrack
annotate1 _ [] = [] 
annotate1 active (e@(MidiEvent t (NoteOn _ n _)) : es) = 
    if instantChange e es || isNothing chord 
        then e : rest 
        else e : (MidiEvent t $! Text $! show $! fromJust chord) : rest
    where active' = S.insert n active
          chord = triad $! S.toList active'
          rest = annotate1 active' es

annotate1 active (e@(MidiEvent t (NoteOff _ n _)) : es) = 
    if instantChange e es || isNothing chord 
        then e : rest 
        else e : (MidiEvent t $! Text $! show $! fromJust chord) : rest
    where active' = S.delete n active
          chord = triad $! S.toList active'
          rest = annotate1 active' es

annotate1 active (e:es) = e : annotate1 active es

dostuff = do
    putStrLn ""
    print $! transposeSignature (2) (-2)
    print $! triad [Note C 1, Note E 1, Note G 1]
    --let path = "/home/patz/Downloads/Gerudo.mid" 
    let path = "/home/patz/Downloads/Never-Gonna-Give-You-Up-1.mid"
    --let path = "/home/patz/Downloads/Gorillaz - Saturnz Barz (Spirit House)  (midi by Carlo Prato) (www.cprato.com).mid"
    --let path = "/home/patz/Downloads/AUD_CT0144.mid"
    midi <- readMidi path
    print $! Midi (header midi) [annotate (tracks midi !! 1)]

test1 :: Test
test1 = TestCase (dostuff)

tests :: Test
tests = TestList [TestLabel "Test1" test1]

main :: IO Counts
main = runTestTT tests
