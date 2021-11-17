{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

-- moduł Midi2 (zmienić)
-- wersja 0.0.2 (ZACZYN)
-- autorzy: T. Obrębski, ..., ..., ...

module Midi2 where

import Euterpea hiding (Message(..),first,second)
import Codec.Midi
import Sound.PortMidi (
                        PMEvent(PMEvent), PMMsg(PMMsg),
                        PMStream,
                        PMError(..),
                        DeviceID,
                        initialize, terminate,
                        openInput, openOutput, close,
                        writeEvents, writeShort, writeSysEx,
                        time,
                        encodeMsg
                      )
import Foreign.C (CLong (CLong),CULong (CULong))
import Data.Word (Word64)
import Data.Bits ((.|.),(.&.),shiftR)
import Data.ByteString.Lazy (unpack)
import GHC.Exts (sortWith)
import Data.Bifunctor (first,second)
import Data.List (intersperse)
import Control.Monad.State
import Control.Monad.Reader


type BPM      = Rational
type RealTime = Float
type PCClock  = Word64


openMidiOutput :: DeviceID -> IO PMStream
openMidiOutput dev = do ans <- openOutput dev 10
                        case ans of
                          Left stream -> return stream
                          Right err -> error (show err)

class PMWritable a where
  send :: PMStream -> a -> IO PMError

instance PMWritable (PCClock, Message) where
  send str (t,msg) | isChannelMessage msg = time >>= \now -> writeShort str $ PMEvent (encodeMsg $ toPMMsg msg) (now + (CULong t))
  send str (t,Sysex n bytes)              = time >>= \now -> writeSysEx str (now + (CULong t)) $ map (toEnum . fromEnum) $ unpack bytes
  send _   _                              = return BadData

instance PMWritable Message where
  send str msg = send str (0::PCClock,msg)

instance PMWritable (Track PCClock) where
  send _ [] = return NoError
  send str (event:track) = send str event >> send str track >>= return

instance PMWritable Schedule where
  send str s = let track = evalState (schedule s) (env0,0)
               in send str (mapToPCClock track)
    where
      mapToPCClock :: Track RealTime -> Track PCClock
      mapToPCClock = map $ first $ round . (* 1000)


-- Env data type

data Env = Env {
                 ch::Channel,     -- kanał MIDI
                 bpm::BPM,        -- tempo wyrażone w "beats per minute"
                 vel::Velocity,   -- domyślna prędkość naduszenia klawisza
                 vel'::Velocity   -- domyślna pręskość puszczenia klawisza
               }

-- manipulacja środowiskiem

setCh      x env               = env { ch   = x   }   -- ustaw kanał
modifyBPM  f env@(Env{bpm=x})  = env { bpm  = f x }   -- zmień BPM funkcją f (f to np. const 60, (/2), (+10))
modifyVel  f env@(Env{vel=x})  = env { vel  = f x }   -- zmień vel funkcją f
modifyVel' f env@(Env{vel'=x}) = env { vel' = f x }   -- zmień vel funkcją f

-- domyślne (startowe) środowisko

env0 = Env {ch=0, bpm=120, vel=64, vel'=64}

-- klasa Schedule
-- reprezentuje działania transformowalne do MIDI

data Schedule = Msg Message                       -- komunikat MIDI
              | Msgs [Message]                    -- lista komunikatów MIDI
              | Local Schedule                    -- wszystkie zmiany Env są lokalne
              | Eut (Music Pitch)                 -- wyrażenie typu (Music Pitch)
              | Channel Channel                   -- ustaw domyślny kanał
              | BPM (BPM->BPM)                    -- zmień BPM (potrzebny np do 
              | Velocity (Velocity->Velocity)
              | Velocity' (Velocity->Velocity)
              | Seq [Schedule]
              | Schedule :> Schedule
              | Schedule :& Schedule
              | Pause Dur
              | After Dur Schedule
              | Every Dur [Schedule]
              | Within Dur [Schedule]


-- monada w której odbywa się translacja Schedule na MIDI (tu: MIDI = Track RealTime)

type TrackState = State (Env,RealTime)

-- pomocnicze funkcje do wyciągania danych z monady TrackState

clock :: TrackState RealTime
clock = snd <$> get
env :: TrackState Env
env = fst <$> get


-- funkcja tłumacząca Schedule na ciąg wydarzeń typu Track RealTime,
-- czas wyrażony w sekundach za pomocą wartości typu RealTime (Float).

schedule :: Schedule -> TrackState (Track RealTime)

-- komunikaty MIDI
-- pojedynczy
schedule (Msg m)       = clock >>= \t -> return [(t,m)]
-- lista do jednoczesnego wysłania
schedule (Msgs ms)     = concat <$> sequence (schedule . Msg <$> ms)

-- 
schedule (Local s)     = do (env,t) <- get
                            let (s',(_,t')) = runState (schedule s) (env,t)
                            put $ (env,t')
                            return s'

-- zmiana środowiska
--upływ czasu
schedule (Pause t)     = env >>= \e -> modify (second $ (+ fromRational (t * 60 / bpm e))) >> return []
-- inne parametry
schedule (Channel f)   = modify (first $ setCh f)      >> return []
schedule (BPM f)       = modify (first $ modifyBPM f)  >> return []
schedule (Velocity f)  = modify (first $ modifyVel f)  >> return []
schedule (Velocity' f) = modify (first $ modifyVel' f) >> return []

-- sekwencje
schedule (Seq ss)      = concat <$> sequence (map schedule ss) >>= return . sortTrack
-- s1, potem s2
schedule (s1 :> s2)    = (++) <$> schedule s1 <*> schedule s2  >>= return . sortTrack
-- s1 jednocześnie z s2
schedule (s1 :& s2)    = do (env,t) <- get
                            let (s1',(env1',t1')) = runState (schedule s1) (env,t)
                                (s2',(env2',t2')) = runState (schedule s2) (env,t)
                            put $ (env1', max t1' t2')
                            return $ sortTrack $ s1' ++ s2'
schedule (After t s)   = schedule $ Pause t :> s 

-- function faciliating spreading MIDI messages across the timeline
schedule (Every t ss)  = schedule $ Seq $ intersperse (Pause t) ss
schedule (Within d ss) = schedule $ Every (d / fromIntegral (length ss - 1)) ss

-- translacja Music Pitch z Euterpei
-- nuta
schedule (Eut (Prim (Note dur pitch))) = env >>= \e -> schedule $ Msg (NoteOn (ch e) (absPitch pitch) (vel e)) :>
                                                                  Pause dur :>
                                                                  Msg (NoteOff (ch e) (absPitch pitch) (vel' e))
-- pauza
schedule (Eut (Prim (Rest dur)))       = schedule (Pause dur)
-- operatory
schedule (Eut (m1 :+: m2))             = schedule (Local (Eut m1) :> Local (Eut m2))
schedule (Eut (m1 :=: m2))             = schedule (Local (Eut m1) :& Local (Eut m2))


sortTrack :: (Num a, Ord a) => Track a -> Track a
sortTrack = sortWith fst

toPMMsg :: Message -> PMMsg
toPMMsg (NoteOff c p v)         = PMMsg (128 .|. (fromIntegral c .&. 0xF)) (fromIntegral p) (fromIntegral v)
toPMMsg (NoteOn c p v)          = PMMsg (144 .|. (fromIntegral c .&. 0xF)) (fromIntegral p) (fromIntegral v)
toPMMsg (KeyPressure c p pr)    = PMMsg (160 .|. (fromIntegral c .&. 0xF)) (fromIntegral p) (fromIntegral pr)
toPMMsg (ControlChange c cn cv) = PMMsg (176 .|. (fromIntegral c .&. 0xF)) (fromIntegral cn) (fromIntegral cv)
toPMMsg (ProgramChange c pn)    = PMMsg (192 .|. (fromIntegral c .&. 0xF)) (fromIntegral pn) 0
toPMMsg (ChannelPressure c pr)  = PMMsg (208 .|. (fromIntegral c .&. 0xF)) (fromIntegral pr) 0
toPMMsg (PitchWheel c pb)       = PMMsg (224 .|. (fromIntegral c .&. 0xF)) (fromIntegral lo) (fromIntegral hi)
  where (hi,lo) = (pb `shiftR` 8, pb .&. 0xFF)
  


-- komunikaty Control Change
ccBankSelect          = (flip ControlChange)   0
ccModulation          = (flip ControlChange)   1 
ccBreathController    = (flip ControlChange)   2
ccFootController      = (flip ControlChange)   4
ccPortamentoTime      = (flip ControlChange)   5
ccVolume              = (flip ControlChange)   7
ccBalance             = (flip ControlChange)   8
ccPan                 = (flip ControlChange)  10
ccExpression          = (flip ControlChange)  11
ccEffectController1   = (flip ControlChange)  12
ccEffectController2   = (flip ControlChange)  13
ccSustain             = (flip ControlChange)  64
ccPortamento          = (flip ControlChange)  65
ccSostenuto           = (flip ControlChange)  66
ccSoftPedal           = (flip ControlChange)  67
ccSegato              = (flip ControlChange)  68
ccHold2               = (flip ControlChange)  69
ccSoundVariation      = (flip ControlChange)  70
ccSoundResonance      = (flip ControlChange)  71
ccSoundRelease        = (flip ControlChange)  72
ccSoundAttack         = (flip ControlChange)  73
ccSoundCutoff         = (flip ControlChange)  74
ccAllSoundsOff        = (flip ControlChange) 120
ccResetAllControllers = (flip ControlChange) 121
ccLocalSwitch         = (flip ControlChange) 122
ccAllNotesOff         = (flip ControlChange) 123
ccOmniOff             = (flip ControlChange) 124
ccOmniOn              = (flip ControlChange) 125
ccMonoMode            = (flip ControlChange) 126
ccPolyMode            = (flip ControlChange) 127



-- NOTATKI

-- w monadzie Reader
-- sc :: (Fractional a, Ord a) => (Schedule a) -> Reader (a,a) (Track a)
-- sc (Msg m)      = ask >>= \(_,time) -> return [(time,m)]           
-- sc (Tempo f s)  = local (first f)      (sc s)
-- sc (After t s)  = local (second (+ t)) (sc s)
-- -- sc (s1 :=: s2)  = (++) <$> sc s1 <*> sc s2 >>= return . sortTrack
-- sc (Every t ss) = sequence (sc . After t <$> ss) >>= return . concat
-- -- sc (Pause t)    = modify (first (+ t)) >> return []




-- sendEvent' :: PMStream -> Event' -> IO PMError

-- midiEvent' :: MidiMessage -> 

-- import Euterpea
-- import Sound.PortMidi (openOutput,
--                        close,
--                        writeEvents,
--                        writeSysEx,
--                        time,
--                        encodeMsg,
--                        PMStream,
--                        PMError,
--                        PMEvent(PMEvent),
--                        PMMsg(PMMsg),
--                        initialize,
--                        terminate)

-- import Foreign.C (CLong (CLong),CULong (CULong))
-- import Data.Maybe (Maybe (Just,Nothing), maybeToList)
-- import Data.Word (Word8)

-- type Byte = Word8
-- type Channel = Byte
-- type Time    = CULong
-- type Sysex   = [Byte]


-- -- zaplanuj wydarzenia na teraz

-- scheduleNow :: [Message] -> [(Time,Message)]
-- scheduleNow = map (0,) 

-- -- rozplanuj wydarzenia równomiernie w ciągu czasu `time' (w sekundach)

-- scheduleWithin :: (RealFrac a) => a -> [Message] -> [(Time,Message)]
-- scheduleWithin time msgs = let n = fromIntegral $ length msgs
--                                interval = round $ (time * 1000) / (n - 1)
--                            in zip [0,interval..] msgs

-- -- rozplanuj wydarzenia co time sekund

-- scheduleEvery :: (RealFrac a) => a -> [Message] -> [(Time,Message)]
-- scheduleEvery time msgs = zip [0,round (time * 1000)..] msgs

-- -- wyślij wydarzenia do wyjścia `stream'

-- sendEvents :: PMStream -> [(Time,Message)] -> IO PMError
-- sendEvents stream events = do now <- time
--                               writeEvents stream $ mkEvents now events

-- -- wyślij komunikat sysex po dalay sekundach

-- sendSysEx' :: PMStream -> Time -> Sysex -> IO PMError
-- sendSysEx' stream delay sysex = do now <- time
--                                    writeSysEx stream (now + delay) $ map (toEnum . fromEnum) sysex

-- sendSysEx :: (RealFrac a) => PMStream -> a -> Sysex -> IO PMError
-- sendSysEx stream delay sysex = do now <- time
--                                   let delay' = round $ delay * 1000
--                                   writeSysEx stream (now + delay') $ map (toEnum . fromEnum) sysex

-- -----------------------------------------------------------------------------------------------------

-- relToAbsTime :: [(Time,Message)] -> [(Time,Message)]


-- encodeMessage :: Message -> Maybe CLong
-- encodeMessage = fmap encodeMsg . midiEvent


-- mkEvent :: Time -> (Time,Message) -> Maybe PMEvent
-- mkEvent base (delay,msg) = encodeMessage msg >>= \e -> Just (PMEvent e (base+delay))

-- mkEvents :: Time -> [(Time,Message)] -> [PMEvent]
-- mkEvents base = concatMap (maybeToList . mkEvent base)



-- -- zapożyczone (skąd ???, z Euterpei ?)



-- byte :: Integer -> Byte
-- byte 0 = 0
-- byte n = fromInteger (n `mod` 2) + 2 * byte (n `div` 10)



-- -- ARCHIWUM

-- -- doSendMessagesDev :: Int -> [Message] -> IO ()
-- -- doSendMessagesDev dev msgs =  do
-- --   stream <- openOutput dev 10
-- --   case stream of
-- --     Right err   -> putStrLn ("After open: " ++ show err)
-- --     Left stream -> sendMessagesNow stream msgs >>= print >> close stream >>= print

-- -- sendMessagesNow :: PMStream -> [Message] -> IO PMError
-- -- sendMessagesNow stream = writeEvents stream . map (($ 0) . PMEvent ) . encodeMessages




-- -- sendEventsDev :: Int -> [(Time,Message)] -> IO ()
-- -- sendEventsDev device events = do stream <- openOutput device 10
-- --                                  case stream of
-- --                                    Right err   -> putStrLn ("After open: " ++ show err)
-- --                                    Left stream -> sendEvents stream events >>= print

-- -- main = do
-- --    let deviceId = 12
-- --    initialize >>= print
-- --    getDeviceInfo deviceId >>= print
-- --    startTime <- time
-- --    let evts = [PMEvent (encodeMsg $ fromJust $ midiEvent (ControlChange 2 3 4)) 0]
-- --    result <- openOutput deviceId 10
-- --    case result of
-- --      Right err   -> putStrLn ("After open: " ++ show err)
-- --      Left stream ->
-- --          do result <- writeEvents stream evts
-- --             putStrLn ("After write: " ++ show result)
-- --             close stream
-- --             return ()
-- --    terminate >>= print

