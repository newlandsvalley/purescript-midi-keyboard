module Main where

import App (Event(..), foldp, initialState, view)
import Control.Monad.Eff.Exception (EXCEPTION)
import Network.HTTP.Affjax (AJAX)
import Audio.SoundFont (AUDIO)
import Control.Monad.Eff (Eff)
import Prelude (Unit, bind, map, ($))
import Pux (CoreEffects, start)
import Pux.Renderer.React (renderToDOM)
import Data.Midi.Instrument (InstrumentName(..))
import Data.Midi.WebMidi (WEBMIDI, Device, webMidiConnect, createDeviceChannel, createEventChannel)
import Data.Midi (TimedEvent)
import Signal (Signal, constant)
import Signal.Channel (CHANNEL, subscribe)


fontSignal :: Signal Event
fontSignal = constant $ RequestLoadFont AcousticGrandPiano

initApp :: ∀ eff. (Eff (ajax :: AJAX, au:: AUDIO, wm :: WEBMIDI | eff) Boolean)
initApp = webMidiConnect

-- | Start and render the app
main :: Eff
        ( channel :: CHANNEL
        , exception :: EXCEPTION
        , ajax :: AJAX
        , wm :: WEBMIDI
        , au :: AUDIO
        )
        Unit
main = do

  webMidiConnected <- initApp

  deviceChannel <- createDeviceChannel
  eventChannel <- createEventChannel
  let
    deviceSubscription = subscribe deviceChannel
    eventSubscription = subscribe eventChannel
    deviceSignal = map DeviceConnection deviceSubscription
    eventSignal = map MidiMessage eventSubscription

  app <- start
    { initialState: initialState webMidiConnected
    , view
    , foldp
    , inputs: [ fontSignal, deviceSignal, eventSignal ]
    }

  renderToDOM "#app" app.markup app.input
