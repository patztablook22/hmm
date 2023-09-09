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

## Functions
`readMidi :: FilePath -> IO Midi`

Reads given MIDI file.
```hs
main = do
    midi <- readMidi "myFile.mid"
```

<br />

`writeMidi :: FilePath -> Midi -> IO()`

Writes given MIDI into file.
```hs
main = do
    let midi = ... :: Midi
    writeMidi "myFile.mid" midi
```

<br />

`semitones :: Note -> Note -> Int`

Returns the interval between two notes in semitones.
```hs
let fifth = semitones (Note C 2) (Note G 2)
-- returns 7

let back = semitones (Note G 2) (Note C 2)
-- returns -7
```

<br />

`transposeNote :: Int -> Note -> Note`

Transposes a note by the given interval in semitones.
```hs
let fifthUp = transpose 7 (Note C 2)
-- returns (Note G 2)

let fifthDn = transpose (-7) (Note C 2)
-- returns (Note F 1)
```

<br />

`transposeSignature :: Int -> Int -> Int`

Transposes by a given interval in semitones (1st argument) the given key signature (2nd argument).
```hs
let twoFlats = -2
let upMaj3rd = 4

let newSignature = transposeSignature upMaj3rd twoFlats
-- returns 2, i.e. 2 sharps
```
<br />

`transposeTrack :: Int -> MidiTrack -> MidiTrack`

Transposes by a given interval the entire MIDI track. This includes all note-related events and all key signature events.
```hs
let track = ... :: MidiTrack
let octave = 12
let track' = transposeTrack octave track
```
<br />

`isTextEvent :: MidiEvent -> Bool` - Returns true iff the given MidiEvent contains Text payload.

`isNameEvent :: MidiEvent -> Bool` - Returns true iff the given MidiEvent contains Name payload.

`isSysExEvent :: MidiEvent -> Bool` - Returns true iff the given MidiEvent contains SysEx payload.

`isNoteOnEvent :: MidiEvent -> Bool` - Returns true iff the given MidiEvent contains NoteOn payload.

`isNoteOffEvent :: MidiEvent -> Bool` - Returns true iff the given MidiEvent contains NoteOff payload.

`isNoteEvent :: MidiEvent -> Bool` - Returns true iff the given MidiEvent contains NoteOn or NoteOff payload.

`isCopyrightEvent :: MidiEvent -> Bool` - Returns true iff the given MidiEvent contains Copyright payload.

`isInstrumentEvent :: MidiEvent -> Bool` - Returns true iff the given MidiEvent contains Instrument payload.

`isUnknownMetaEvent :: MidiEvent -> Bool` - Returns true iff the given MidiEvent contains UnknownMeta payload.

<br />

`merge :: MidiTrack -> MidiTrack -> MidiTrack`

Merges two MIDI tracks into one.
```hs
let pianoLeftHand = ... :: MidiTrack
let pianoRightHand = ... :: MidiTrack

let pianoBothHands = merge pianoLeftHand pianoRightHand
```

<br />

`midiTrackName :: MidiTrack -> Maybe String`

Extracts the track's name, if provided by a Name event.

<br />

`midiNames :: Midi -> [String]`

Returns the names of all named MidiTracks.

<br />

`midiTrackInstruments :: MidiTrack -> [String]`

Returns the names of all instruments in a single MidiTrack.

<br />

`midiInstruments :: Midi -> String`

Returns the names of all instruments in the entire MIDI.

<br />

`midiCopyright :: Midi -> Maybe String`

Returns the MIDI's copyright if present.

<br />

`rootedChord :: [Note] -> Maybe Chord`

Heuristically interprets given (unordered) list of notes as a rooted chord. 
```hs
let notes = [Note Fsharp 3,
             Note D 3,
             Note E 2,
             Note B 1,
             Note C 1]

let notes' = [Note Csharp 4,
              Note G 3,
              Note Fsharp 3,
              Note E 2]

let notes'' = [Note G 3]

let chord = rootedChord notes
-- returns Just (Chord C Maj [Natural 9, Sharp 11])

let chord' = rootedChord notes'
-- returns Just (Chord E Min [Natural 6, Natural 2])

let chord'' = rootedChord notes''
-- returns Nothing
```

<br />

`annotateChords :: ([Note] -> Maybe Chord) -> MidiTrack -> MidiTrack`

Adds a chord annotation (Text event containing string chord representation) into the given track whenever the set of currently active notes changes and the annotator function returns (Just _).
```hs
let track = ... :: MidiTrack
let annotator = rootedChord
let track' = annotateChords annotator track
```

## Data

`PitchClass` 

Represents the cross-octave pitch class, e.g. `C`, `Csharp`, `D`. This type is concerned purely with pitch, not with theoretical interpretation. Therefore, since MIDI uses the equal temperament, enharmonic equivalence applies, i.e. instead of `Dflat` (which is not provided), use `Csharp`.

<br />

`Note`

Represents a single note. Consists of its `PitchClass` and its `Octave` (represented as an `Int`).
```hs
let note = Note C 2
let note' = Note Asharp 5
```

<br />

`Midi`

Representation of an entire MIDI object. Consists of
- `header :: MidiHeader`
- `tracks :: [MidiTrack]`

<br />

`MidiFormat` 

See MIDI specification for MIDI formats.

<br />

`MidiHeader`

See MIDI specification for the MIDI header. Consists of
- `format :: MidiFormat`
- `ntracks :: Int`
- `division :: Int`

<br />

`ChurchMode`

Represents the diatonic church modes, most importantnly `Ionian` ("Major") and `Aeolian` ("Minor").

<br />

`MidiTrack`

Represents a single midi track - a list of ordered `MidiEvent`s.

<br />

`MidiEvent`

An element of `MidiTrack` - consists of its timestamp and `Event` payloads.

<br />

`Event`

The payload of a `MidiEvent`. Can be of many types, such as `NoteOn`, `Text`, `Copyright`, `PitchWheel`.

<br />

`Chord`

A jazz theory influenced representation of a chord. Consists of
- its `PitchClass`
- its `ChordType`, e.g. `Maj`, `Min`, `Sus4`
- its relevant (the highest natural and all altered) `[Extension]`, e.g. `[Flat 7]`

see e.g. `rootedChord`.
