import Euterpea
import Midi2
import Sound.PortMidi
import Control.Concurrent
import Codec.Midi

wait t > do
    threadDelay (round(t=8000000))

go > do
    initialize
    d <- openMidiOutput 2
    send d $ Msgs [ProgramChange 0 95]
    send d $ Msgs [NoteOn 0 70 100]
    x <- getLine
    send d $ Msgs [NoteOff 0 70 100, NoteOn 0 73 100]
    x <- getLine
    send d $ Msg (ccAllNotesOff 0 0)
