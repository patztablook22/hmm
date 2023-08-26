--
-- MIDI format specification used: http://www.music.mcgill.ca/~ich/classes/mumt306/StandardMIDIfileformat.html
-- 


module Hmm
( readMidi, writeMidi
, Midi(..), MidiHeader(..), MidiTrack(..), MidiEvent(..), Event(..)
, PitchClass(..), Octave(..), Note(..)
, semitones, transposeNote, transposeSignature, transposeTrack
, isTextEvent, isNameEvent, isSysExEvent, isNoteOnEvent, isNoteOffEvent
, isCopyrightEvent, isInstrumentEvent, isUnknownMetaEvent
, merge, annotateChords
, midiTrackName, midiNames, midiTrackInstruments, midiInstruments
, midiCopyright
, rootedChord
, Chord(..), ChordType(..), Extension(..)
) where


import Data.List
import Data.Maybe
import Control.Monad
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as BsL
import qualified Data.ByteString as Bs
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Char

readMidi :: FilePath -> IO Midi
readMidi path = do
    input <- BsL.readFile path
    return $! runGet getMidi input

writeMidi :: FilePath -> Midi -> IO ()
writeMidi path midi = do
    let bytes = runPut $! putMidi midi
    BsL.writeFile path bytes

-- Enharnmonic equivalents included implicitly.
data PitchClass = C | Csharp | D | Dsharp | E | F | Fsharp | G | Gsharp
                | A | Asharp | B
                deriving (Show, Eq)

type Octave = Int
data Note = Note PitchClass Octave deriving (Show, Eq)

-- See MIDI specification.
code2note :: Int -> Note
code2note code = Note pc octave
    where octave = div code 12 - 1
          pc = case mod code 12 of 0  -> C
                                   1  -> Csharp
                                   2  -> D
                                   3  -> Dsharp
                                   4  -> E
                                   5  -> F
                                   6  -> Fsharp
                                   7  -> G
                                   8  -> Gsharp
                                   9  -> A
                                   10 -> Asharp
                                   11 -> B

-- See MIDI specification.
note2code :: Note -> Int
note2code (Note pc octave) = n + 12 * (octave + 1)
    where n = case pc of C      -> 0
                         Csharp -> 1
                         D      -> 2
                         Dsharp -> 3
                         E      -> 4
                         F      -> 5
                         Fsharp -> 6
                         G      -> 7
                         Gsharp -> 8
                         A      -> 9
                         Asharp -> 10
                         B      -> 11

-- Compare notes by pitch.
instance Ord Note where
    compare a b = compare (note2code a) (note2code b)

-- See MIDI specification.
data MidiFormat = SingleTrack | MultiTrack | MultiSong deriving (Eq, Show)

-- See MIDI specification.
data MidiHeader = MidiHeader
    { format            :: MidiFormat
    , ntracks           :: Int
    , division          :: Int
    } deriving Show

-- See MIDI specification.
-- For all following getXyz/putXyz functions.

getMidiFormat :: Get MidiFormat
getMidiFormat = do
    nFormat <- getWord16beAs
    return (case nFormat of
             0 -> SingleTrack
             1 -> MultiTrack
             2 -> MultiSong)

getWord8As = fromIntegral <$> getWord8
getWord16beAs = fromIntegral <$> getWord16be
getWord32beAs = fromIntegral <$> getWord32be

getMidiHeader :: Get MidiHeader
getMidiHeader = do
    chunkType <- getByteString 4 -- "MThd"
    chunkLen <- getWord32beAs
    format <- getMidiFormat
    tracks <- getWord16beAs
    div <- getWord16beAs
    return $! MidiHeader format tracks div

-- MIDI only requires major/minor (Ionian/Aeolian), 
-- but this is a good generalization.
data ChurchMode = Lydian | Ionian | Mixolydian | Dorian
                | Aeolian | Phrygian | Locrian
                deriving (Show, Eq)

type MidiTrack = [MidiEvent]
data MidiEvent = MidiEvent Integer Event deriving Show

type Channel = Int
data Event = NoteOn Channel Note Int
           | NoteOff Channel Note Int
           | KeyPressure Channel Note Int
           | ChannelPressure Channel Int
           | ControlChange Channel Int Int
           | ProgramChange Channel Int
           | PitchWheel Channel Int
           | SysEx [Int]
           | Text String
           | Copyright String
           | Name String
           | Instrument String
           | TimeSignature Int Int Int Int
           | Tempo Int
           | SmpteOffset Int Int Int Int Int
           | KeySignature Int ChurchMode
           | EndOfTrack
           | UnknownMeta Int [Int]
           | ReadError [Int]
           deriving (Show, Eq)

-- A sequence of bytes, each containing 
--   7 bits of the quantity,
--   1-bit continue flag.
getVariableLengthQuantity :: Integral a => Get a
getVariableLengthQuantity = do
    byte <- fromIntegral <$> getWord8
    let value = mod byte 128
    let bit7 = div byte 128
    if bit7 == 0
        then return value
        else do
            rest <- getVariableLengthQuantity
            return $! value * 128 + rest

putVariableLengthQuantity :: Integral a => a -> Put
putVariableLengthQuantity value = do
    let data' = getData value
    putByteString $! Bs.pack $! map fromIntegral data'
    where getData value = if value < 128
                          then [value]
                          else ((128 + div value 128) : (getData $! mod value 128))

getNote = code2note <$> getWord8As

-- There is a MIDI format feature that allows to skip the current 
-- MIDI event status if it is the same as the previous one, 
-- so it has to be deduced manually... yay.
getMidiEventStatus :: Maybe (Integer, Int) -> Get Int
getMidiEventStatus (Just (_, prev)) = do
    status <- lookAhead getWord8As
    if div status 128 == 1 
        then do 
            _ <- getWord8
            return status 
        else return prev
getMidiEventStatus Nothing = do
    status <- getWord8As
    return $! if div status 128 == 1 then status else undefined

-- This function does not cover all MIDI event types, 
-- as there is a gzillion of them.
-- But it covers the useful ones.
getMidiEvent :: Maybe (Integer, Int) -> Get (MidiEvent, (Integer, Int))
getMidiEvent prev = do
    let timePrev = case prev of (Just (tp, _)) -> tp; _ -> 0
    time <- (timePrev+) <$> getVariableLengthQuantity
    status <- getMidiEventStatus prev
    case status of
        0xFF -> do
            type' <- getWord8As
            len <- getVariableLengthQuantity
            data' <- replicateM len getWord8As

            return $! (MidiEvent 
                        time
                        (case type' of
                            0x01 -> Text $! map chr data'
                            0x02 -> Copyright $! map chr data'
                            0x03 -> Name $! map chr data'
                            0x58 -> TimeSignature (data'!!0) (data'!!1) 
                                                  (data'!!2) (data'!!3)
                            0x51 -> let f a b = a * 256 + b 
                                    in Tempo $! foldl f 0 data'
                            0x54 -> SmpteOffset (data'!!0) (data'!!1) (data'!!2)
                                                (data'!!3) (data'!!4)
                            0x59 -> KeySignature (data'!!0)
                                                 (if data'!!1 == 0 
                                                  then Ionian
                                                  else Aeolian)
                            0x2f -> EndOfTrack
                            _    -> UnknownMeta type' data'
                        ), (time, status))

        0xF0 -> do
            len <- getVariableLengthQuantity
            data' <- replicateM len getWord8As
            return $! (MidiEvent time (SysEx data'), (time, status))

        0xF7 -> do
            len <- getVariableLengthQuantity
            data' <- replicateM len getWord8As
            return $! (MidiEvent time (SysEx data'), (time, status))

        _    -> do
            let leading4 = div status 0x10
            let trailing4 = mod status 0x10
            let channel = fromIntegral trailing4

            event <- case leading4 of
                0x8 -> do 
                    note <- getNote
                    velocity <- getWord8As
                    return $! NoteOff channel note velocity
                0x9 -> do 
                    note <- getNote
                    velocity <- getWord8As
                    return $! NoteOn channel note velocity
                0xa -> do
                    note <- getNote
                    velocity <- getWord8As
                    return $! KeyPressure channel note velocity
                0xb -> do
                    controller <- getWord8As
                    value <- getWord8As
                    return $! ControlChange channel controller value
                0xc -> do
                    program <- getWord8As
                    return $! ProgramChange channel program
                0xd -> do
                    pressure <- getWord8As
                    return $! ChannelPressure channel pressure
                0xe -> do
                    lsbits <- getWord8As
                    msbits <- getWord8As
                    return $! PitchWheel channel (msbits * 128 + lsbits)
                _   -> do
                    return $! ReadError [status, leading4, trailing4]

            return $! (MidiEvent time event, (time, status))

-- Consumes the entire byte sequence (potentially `isolate`d) as MIDI events.
getMidiEvents :: Maybe (Integer, Int) -> Get [MidiEvent]
getMidiEvents prev = do
    end <- isEmpty
    if end
        then return []
        else do
            (event@(MidiEvent time' ev), next) <- getMidiEvent prev
            rest <- case ev of (ReadError _) -> do
                                                buff <- getRemainingLazyByteString
                                                return []
                               _ -> getMidiEvents (Just next)
            return (event : rest)

getMidiTrack :: Get MidiTrack
getMidiTrack = do
    chunkType <- getByteString 4 -- "MTrk"
    len <- getWord32beAs
    isolate len (getMidiEvents Nothing)

data Midi = Midi
    { header :: MidiHeader
    , tracks :: [MidiTrack]
    }

-- For fancy printing.
instance Show Midi where
    show midi = 
        "Midi {header = " ++ show (header midi) ++ ",\n      tracks = [" ++
        foldr1 (page 16) (map showTrack (tracks midi))
        ++ "]\n     }"
        where page pad a b = a ++ ",\n" ++ replicate pad ' ' ++ b
              showTrack :: MidiTrack -> String
              showTrack track = "[" ++ foldr1 (page 17) (map show track) ++ "]"

getMidi :: Get Midi
getMidi = do
    header <- getMidiHeader
    tracks <- replicateM (ntracks header) getMidiTrack
    return $! Midi header tracks
        
-- The interval between two notes in semitones.
semitones :: Note -> Note -> Int
semitones n1 n2 = (note2code n2) - (note2code n1)

-- Transposes a note up/down.
transposeNote :: Int -> Note -> Note
transposeNote semitones = code2note . (+semitones) . note2code

-- Transposes a signature using the circle of fiths
--   e.g. transposing 1 flat up by 2 semitones (1 tone) 
--   means going up by 2 fifths, so the resulting signature is 1 sharp.
transposeSignature semitones sf = let sf' = mod (12 + (mod (sf + semitones * 7) 12)) 12
                                      sf'' = if sf' <= 5 then sf' else sf' - 12
                                  in sf''

putMidi :: Midi -> Put
putMidi midi = do
    putMidiHeader $! header midi
    mapM_ putMidiTrack $! tracks midi

putMidiFormat SingleTrack = putWord16be 0
putMidiFormat MultiTrack = putWord16be 1
putMidiFormat MultiSong = putWord16be 2

putAsWord8 = putWord8 . fromIntegral
putAsWord16be = putWord16be . fromIntegral
putAsWord32be = putWord32be . fromIntegral
putAsciiString = mapM_ (putAsWord8 . ord)

putMidiHeader :: MidiHeader -> Put
putMidiHeader header = do
    putAsciiString "MThd"
    putAsWord32be 6
    putMidiFormat $! format header
    putAsWord16be $! ntracks header
    putAsWord16be $! division header

putMidiTrack :: MidiTrack -> Put
putMidiTrack track = do
    putAsciiString "MTrk"
    let eventsBytes = runPut $! putEvents 0 track
    putAsWord32be $! BsL.length eventsBytes
    putLazyByteString eventsBytes
    where putEvents _ [] = return ()
          putEvents timePrev (e@(MidiEvent time _):es) = do
            putMidiEvent timePrev e
            putEvents time es

putVariableLengthBytes :: Integral a => [a] -> Put 
putVariableLengthBytes ints = do
    let bytes = runPut $! putByteString $! Bs.pack $! map fromIntegral ints
    putVariableLengthQuantity $! BsL.length bytes
    putLazyByteString bytes

putVariableLengthString :: [Char] -> Put 
putVariableLengthString = putVariableLengthBytes . (map ord)

putFixedLengthBytes :: Integral a => [a] -> Put
putFixedLengthBytes ints = do
    let bytes = runPut $! putByteString $! Bs.pack $! map fromIntegral ints
    putLazyByteString bytes

putNote :: Note -> Put
putNote = putWord8 . fromIntegral . note2code

-- Again, only implementing some crucial events;
-- otherwise it would just be too much work for one project.
putMidiEvent :: Integer -> MidiEvent -> Put
putMidiEvent timePrev (MidiEvent time event) = do
    putVariableLengthQuantity $! time - timePrev
    case event of
        SysEx data' -> do
            putAsWord8 0xf0
            putVariableLengthBytes data'
        NoteOn channel note velocity -> do
            putAsWord8 $! 0x90 + channel
            putNote note
            putAsWord8 velocity
        NoteOff channel note velocity -> do
            putAsWord8 $! 0x80 + channel
            putNote note
            putAsWord8 velocity
        KeyPressure channel note velocity -> do
            putAsWord8 $! 0xa0 + channel
            putNote note
            putAsWord8 velocity
        ControlChange channel controller value -> do
            putFixedLengthBytes [0xb0 + channel, controller, value]
        ProgramChange channel program -> do
            putFixedLengthBytes [0xc0 + channel, program]
        ChannelPressure channel pressure -> do
            putFixedLengthBytes [0xd0 + channel, pressure]
        PitchWheel channel change -> do
            putFixedLengthBytes [0xc0 + channel, mod change 128, div change 128]
        Text string -> do
            putFixedLengthBytes [0xff, 0x01]
            putVariableLengthString string
        Copyright string -> do
            putFixedLengthBytes [0xff, 0x02]
            putVariableLengthString string
        Name string -> do
            putFixedLengthBytes [0xff, 0x03]
            putVariableLengthString string
        SmpteOffset hr mm se fr ff -> do
            putFixedLengthBytes [0xff, 0x54, 0x05, hr, mm, se, fr, ff]
        TimeSignature nn dd cc bb -> do
            putFixedLengthBytes [0xff, 0x58, 0x04, nn, dd, cc, bb]
        Tempo t -> do
            putFixedLengthBytes [0xff, 0x51, 0x03, div t 0x10000, mod (div t 0x100) 0x100, mod t 0x100]
        KeySignature sf mode -> do
            let modeCode = case mode of
                            Ionian -> 0
                            Aeolian -> 1
            putFixedLengthBytes [0xff, 0x02, sf, modeCode]
        EndOfTrack -> do
            putFixedLengthBytes [0xff, 0x2f, 0x00]
        UnknownMeta type' data' -> do
            putFixedLengthBytes [0xff, type']
            putVariableLengthBytes data'
        _ -> do return ()

-- Helpful predicates for filtering events follow,
-- e.g. `filter isTextEvent midiTrack` to extract all text events,
--      `filter isNoteEvent midiTrack` to extract all note on/off events.

isTextEvent (MidiEvent _ (Text _)) = True
isTextEvent _ = False

isNameEvent (MidiEvent _ (Name _)) = True
isNameEvent _ = False

isNoteOnEvent (MidiEvent _ (NoteOn _ _ _)) = True
isNoteOnEvent _ = False

isNoteOffEvent (MidiEvent _ (NoteOff _ _ _)) = True
isNoteOffEvent _ = False

isNoteEvent e = isNoteOnEvent e || isNoteOffEvent e

isSysExEvent (MidiEvent _ (SysEx _)) = True
isSysExEvent _ = False

isCopyrightEvent (MidiEvent _ (Copyright _)) = True
isCopyrightEvent _ = False

isInstrumentEvent (MidiEvent _ (Instrument _)) = True
isInstrumentEvent _ = False

isUnknownMetaEvent (MidiEvent _ (UnknownMeta _ _)) = True
isUnknownMetaEvent _ = False

-- Merges two MIDI tracks into one.
-- E.g. to add the bassline into the piano track:
-- `merged = merge basslineTrack pianoTrack`
merge :: MidiTrack -> MidiTrack -> MidiTrack
merge (e1@(MidiEvent time1 _):es1) (e2@(MidiEvent time2 _):es2)
    | time1 <= time2 = (e1 : merge es1 (e2:es2))
    | otherwise = (e2 : merge (e1:es1) es2)
merge [] es2 = es2
merge es1 [] = es1

-- Helper function for accumulating metadata collected from a MIDI track.
getMetaMany :: (MidiEvent -> Maybe a) -> MidiTrack -> [a]
getMetaMany getter es = [j | Just j <- map getter es]

-- Returns contents of all Name events in a track.
midiTrackName :: MidiTrack -> Maybe String
midiTrackName = listToMaybe . getMetaMany getter
    where getter (MidiEvent 0 (Name s)) = Just s
          getter _ = Nothing

-- Returns contents of all Name events in a MIDI.
midiNames :: Midi -> [String]
midiNames = catMaybes . map midiTrackName . tracks

-- Returns contents of all Instrument events in a track.
midiTrackInstruments :: MidiTrack -> [String]
midiTrackInstruments = getMetaMany getter
    where getter (MidiEvent _ (Instrument s)) = Just s
          getter _ = Nothing

-- Returns contents of all Instrument events in a MIDI.
midiInstruments :: Midi -> [String]
midiInstruments =  concatMap midiTrackInstruments . tracks

-- Returns MIDI copyright if present.
midiCopyright :: Midi -> Maybe String
midiCopyright = listToMaybe . concatMap (getMetaMany getter) . tracks
    where getter (MidiEvent 0 (Copyright s)) = Just s
          getter _ = Nothing

-- Transposes all note-related events;
-- includes note on/off events, key pressure events, key signature events.
transposeTrack :: Int -> MidiTrack -> MidiTrack
transposeTrack semitones = map f
    where tn = transposeNote semitones
          f (MidiEvent t (NoteOn channel note velocity)) =
            (MidiEvent t (NoteOn channel (tn note) velocity))
          f (MidiEvent t (NoteOff channel note velocity)) = 
            (MidiEvent t (NoteOff channel (tn note) velocity))
          f (MidiEvent t (KeyPressure channel note velocity)) = 
            (MidiEvent t (KeyPressure channel (tn note) velocity))
          f (MidiEvent t (KeySignature sf mode)) = 
            (MidiEvent t (KeySignature (transposeSignature semitones sf) mode))
          f e = e

data Chord = Chord PitchClass ChordType [Extension] deriving Show
data ChordType = Min | Maj | MinMaj | Dom | Aug | Dim | HalfDim 
               | Sus2 | Sus4
               deriving (Show, Eq)
data Extension = Natural Int | Sharp Int | Flat Int deriving Eq
instance Show Extension where
    show (Natural n) = show n
    show (Sharp n) = "Sharp " ++ show n
    show (Flat n) = "Flat " ++ show n 

rootedChord :: [Note] -> Maybe Chord
rootedChord [] = Nothing
rootedChord notes = let (r@(Note root _) : rest) = sort notes
                        intervals = map ((`mod` 12) . semitones r) rest
                        hasThird = elem 3 intervals || elem 4 intervals
                        hasMinorThird = elem 3 intervals 
                        hasMinorSeventh = elem 10 intervals
                        hasMajorSeventh = elem 11 intervals
                        hasSeventh = hasMinorSeventh || hasMajorSeventh
                        hasDimFifth = elem 6 intervals
                        hasAugFifth = elem 8 intervals
                        hasSixth = elem 9 intervals
                        ct = if hasThird then
                                if hasMinorThird then
                                    if hasDimFifth then
                                        if hasMinorSeventh then HalfDim else Dim
                                    else if hasMajorSeventh then MinMaj 
                                    else Min
                                else
                                    if hasAugFifth then Aug
                                    else if hasMinorSeventh then Dom
                                    else Maj
                             else if elem 5 intervals then Sus4
                             else Sus2

                        exts = sortOn deg $! nub $! filter (/= Natural 0) $! map f intervals

                        deg (Natural n) = 2 * n
                        deg (Sharp n) = 2 * n + 1
                        deg (Flat n) = 2 * n - 1

                        f :: Int -> Extension
                        f 0 = Natural 0
                        f 1 = Flat 9
                        f 2 = if ct == Sus2 then Natural 0 else Natural 9
                        f 3 = if elem ct [Maj, Aug, Dom] then Sharp 9 else Natural 0
                        f 4 = Natural 0
                        f 5 = if ct == Sus4 then Natural 0 else Natural 11
                        f 6 = if elem ct [Dim, HalfDim] then Natural 0 else Sharp 11
                        f 7 = Natural 0
                        f 8 = if elem ct [Aug] then Natural 0
                              else if hasSeventh then Flat 13
                              else Flat 6
                        f 9 = if hasSeventh then Natural 13 else Natural 6
                        f 10 = if elem ct [Dom, Min, HalfDim] then Natural 7
                               else Flat 7
                        f 11 = if elem ct [Maj, MinMaj, Aug] then Natural 7
                               else Sharp 7

                        isNaturalExt (Natural n) = odd n
                        isNaturalExt _ = False
                        highestNatural = listToMaybe . reverse . filter isNaturalExt

                        compress (Natural 6 : exts) = (Natural 6 : compress exts)
                        compress (Flat 6 : exts) = (Flat 6 : compress exts)
                        compress exts = case highestNatural exts of
                                            Nothing -> exts
                                            Just hn -> (hn : filter (not . isNaturalExt) exts)

                    in Just $ Chord root ct (compress exts)

annotateChords = annotate1 S.empty
annotate1 :: S.Set Note -> ([Note] -> Maybe Chord) -> MidiTrack -> MidiTrack
annotate1 _ _ [] = [] 
annotate1 active annotator (e@(MidiEvent t (NoteOn _ n _)) : es) = 
    if instantChange e es || isNothing chord 
        then e : rest 
        else e : (MidiEvent t $! Text $! show $! fromJust chord) : rest
    where active' = S.insert n active
          chord = annotator $! S.toList active'
          rest = annotate1 active' annotator es

annotate1 active annotator (e@(MidiEvent t (NoteOff _ n _)) : es) = 
    if instantChange e es || isNothing chord 
        then e : rest 
        else e : (MidiEvent t $! Text $! show $! fromJust chord) : rest
    where active' = S.delete n active
          chord = annotator $! S.toList active'
          rest = annotate1 active' annotator es

annotate1 active annotator (e:es) = e : annotate1 active annotator es

instantChange (MidiEvent t1 _) ((MidiEvent t2 _):_) = t1 == t2
instantChange _ _ = False
