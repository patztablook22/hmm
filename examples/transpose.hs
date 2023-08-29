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

