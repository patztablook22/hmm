## About
Hmm (Haskell MIDI Manipulator) is a Haskell library that allows you to
- Read MIDI files
- Functionally manipulate MIDI in memory
- Write MIDI files

## Examples

### Dump MIDI info

```hs
-- examples/info.hs

import Hmm
import System.Environment

main = do
    args <- getArgs
    let path = head args
    midi <- readMidi path
    print $ header midi
    print $ midiNames midi
```
Example run:
```sh
./info never-gonna-give-you-up.mid
```
Output:
```
MidiHeader {format = MultiTrack, ntracks = 17, division = 384}
["E.PIANO 2","SYN BASS 2","CLEAN GTR","MELODY","PICCOLO","SYNTH DRUM","SYNTH DRUM","SAW WAVE","SOPRAN SAX","DRUMS","STRINGS","TRUMPET","BRASS 1","WHISTLE","MUTED GTR","GS/RESET"]
```

### Transpose MIDI

```hs
-- examples/transpose.hs

import Hmm
import System.Environment

main = do
    args <- getArgs
    let (semitonesStr : input : output : _) = args
    let semitones = read semitonesStr :: Int
    midi <- readMidi input
    let midi' = Midi {header = header midi,
                      tracks = map (transposeTrack semitones) (tracks midi)}
    writeMidi output midi'
```
Example run:
```sh
./transpose 7 never-gonna-give-you-up.mid never-gonna-give-you-up-transposed.mid
```
Now, `never-gonna-give-you-up-transposed.mid` contains the original track transposed up by 7 semitones (i.e. the perfect fifth).

## Installation

```sh
# Clone the repo
git clone https://github.com/patztablook22/hmm

# Use cabal to build and install it
cd hmm
cabal build
cabal install --lib
```
