module Main where

import App (Event(..), foldp, initialState, view)
import Effect (Effect)
import Prelude (Unit, bind, map, ($))
import Pux (start)
import Pux.Renderer.React (renderToDOM)
import Data.Midi.Instrument (InstrumentName(..))
import Data.Midi.WebMidi (webMidiConnect, createDeviceChannel, createEventChannel)
import Signal (Signal, constant)
import Signal.Channel (subscribe)


fontSignal :: Signal Event
fontSignal = constant $ RequestLoadFont AcousticGrandPiano

initApp :: Effect Boolean
initApp = webMidiConnect

-- | Start and render the app
main :: Effect Unit
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
