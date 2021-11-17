import Midi2
import Sound.PortMidi
import Codec.Midi
import Control.Concurrent

wait t = do
   threadDelay (round (t*1000000))
   
go = do
   initialize
   d <- openMidiOutput 2
   send d $ Msg (ProgramChange 0 95)
   x <- getLine
   send d $ Msgs [NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 51 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 51 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 51 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 53 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 51 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 51 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 51 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   --koniec linii 2
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 53 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 50 100]
   --koniec linii 3
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 47 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 47 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 47 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 47 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 47 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 47 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 44 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 47 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 47 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 44 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 47 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 47 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 44 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 47 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 47 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 44 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 46 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 47 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 49 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 50 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 52 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 53 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 55 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 49 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 50 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 51 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 52 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 53 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 53 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 51 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0, NoteOn 0 48 100]
   x <- getLine
   send d $ Msgs [ccAllNotesOff 0 0]
   terminate
   
   
   
   
   
  
