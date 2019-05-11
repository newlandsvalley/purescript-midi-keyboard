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

data Action =
    Init
  | HandleChangeInstrument (Maybe InstrumentName)
  | HandleDeviceConnection Device
  | HandleMidiEvent Midi.TimedEvent
