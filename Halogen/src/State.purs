module State where

-- | The Container State

import Audio.SoundFont (Instrument)
import Data.Midi.Instrument (InstrumentName)
import Data.Midi.WebMidi (Device)
import Data.Midi (TimedEvent) as Midi
import Data.Map (Map)
import Data.Maybe (Maybe)

type Devices = Map String Device

type State = {
   webMidiConnected :: Boolean
 , inputDevices :: Devices
 , instruments :: Array Instrument
 , maxVolume :: Int
}

data Query a =
    Init a
  | HandleChangeInstrument (Maybe InstrumentName) a
  | HandleDeviceConnection Device a
  | HandleMidiEvent Midi.TimedEvent a

-- there are no components in the container so ChildSlots is empty
type ChildSlots =
  ()
