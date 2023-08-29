import Hmm
import System.Environment

main = do
    args <- getArgs
    let path = head args
    midi <- readMidi path
    print $ header midi
    print $ midiNames midi
