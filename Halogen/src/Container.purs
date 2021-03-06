module Container where

import Prelude

import Audio.SoundFont (MidiNote, loadRemoteSoundFonts, playNote)
import Data.Midi (TimedEvent(..), Event(..)) as Midi
import Data.Midi.WebMidi (Device, createDeviceChannel, createEventChannel, webMidiConnect)
import Effect.Aff.Class (class MonadAff)
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.Int (toNumber)
import Data.Map (delete, empty, insert)
import Data.Midi.Instrument (InstrumentName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource (Emitter, Finalizer, EventSource, effectEventSource, emit)
import Render (renderConnection, renderInstruments, renderInstrumentMenu)
import Signal (Signal, runSignal, (~>))
import Signal.Channel (subscribe) as Chan
import State (State, Action(..))

-- | volumes in MIDI range from 0 to 127
volumeCeiling :: Int
volumeCeiling = 127

-- although this module is named 'Container' there are no child slots
-- ChildSlots is just ()


component :: ∀ i o q m. MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        , finalize = Nothing
        }
    }
  where

  initialState :: i -> State
  initialState _ =
     { webMidiConnected : false
     , inputDevices : empty
     , instruments : []
     , maxVolume : (volumeCeiling / 2)  -- start at half volume ceiling
     }

  render :: State -> H.ComponentHTML Action () m
  render state = HH.div_
    [ HH.h1
      [HP.class_ (H.ClassName "center") ]
      [HH.text "MIDI Keyboard"]
    , HH.div
      -- left pane
      [ HP.class_ (H.ClassName "leftPane") ]
      [
        renderInstruments state.instruments
      , renderConnection state
      , renderInstrumentMenu state.instruments
      ]
    ]

  handleAction ∷ Action → H.HalogenM State Action () o m Unit
  handleAction = case _ of
     Init -> do
       -- connect to web-MIDI
       webMidiConnected <- H.liftEffect webMidiConnect
       -- load the initial instrument font
       instruments <- H.liftAff $ loadRemoteSoundFonts [AcousticGrandPiano]
       _ <- H.modify (\st -> st { webMidiConnected = webMidiConnected
                                , instruments = instruments} )

       -- subscribe to device connections and disconnections
       deviceSource <- H.liftEffect deviceEventSource
       _ <- H.subscribe deviceSource
       -- subscribe to MIDI event messages from these devices
       midiSource <- H.liftEffect midiEventSource
       _ <- H.subscribe midiSource
       pure unit
     HandleChangeInstrument mInstrumentName -> do
       case mInstrumentName of
         Just instrumentName ->
           do
             -- wipe out current instruments to invoke a 'wait' message in the HTMP
             _ <- H.modify (\st -> st { instruments = [] })
             -- load the requested instrument
             instruments <- H.liftAff $ loadRemoteSoundFonts [instrumentName]
             _ <- H.modify (\st -> st { instruments = instruments} )
             pure unit
         _ ->
           pure unit
     HandleDeviceConnection device -> do
       state <- H.get
       let
         newDevices =
           if device.connected then
             insert device.id device state.inputDevices
           else
             delete device.id state.inputDevices
       _ <- H.modify (\st -> st { inputDevices = newDevices } )
       pure unit
     HandleMidiEvent timedEvent -> do
       state <- H.get
       newState <- playMidiEvent timedEvent state
       H.put newState
       pure unit



  -- | interpret MIDI event messages
  -- | at the moment we only respond to:
  -- |    NoteOn
  -- |    Control Volume
  -- |
  -- | This multiplexes all MIDI channels to the single font we have loaded
  -- | i.e. if you have various devices connected then they all play though
  -- | the currently loaded instrument
  -- |
  -- | but obviously this is easily extended to other messages
  -- | Also, volume control discriminate neither which
  -- | device is being played nor which MIDI channel is in operation
  -- | (i.e. you're probably OK if you just attach a single device)
  playMidiEvent :: Midi.TimedEvent -> State ->  H.HalogenM State Action () o m State
  playMidiEvent (Midi.TimedEvent te) state =
    case te.event of
      Just (Midi.NoteOn channel pitch velocity) ->
        do
          let
            -- respond to the current volume control setting
            volumeScale =
              toNumber state.maxVolume / toNumber volumeCeiling
            -- and this is what's left of the note
            gain =
              toNumber velocity * volumeScale / toNumber volumeCeiling
            midiNote :: MidiNote
            -- use the channel 0 font for all NoteOn messahes
            midiNote = { channel : 0, id: pitch, timeOffset: 0.0, duration : 1.0, gain : gain }
          _ <- H.liftEffect $ playNote state.instruments midiNote
          pure state
      _ ->
        pure $ recogniseControlMessage te.event state

  -- | recognise and act on a control message and save to the model state
  -- |    At the moment, we just recognise volume changes
  recogniseControlMessage :: Maybe Midi.Event -> State -> State
  recogniseControlMessage mevent state =
    case mevent of
      Just (Midi.ControlChange channel 7 amount) ->
        state { maxVolume = amount }
      _ ->
        state

  -- provide a function that can be fred to effectEventSource for adapting a Signal
  -- to a Halogen Query Event (as if it came, say, from a DOM Event)
  adaptDeviceSignal :: Signal Device -> (Emitter Effect Action -> Effect (Finalizer Effect))
  adaptDeviceSignal sig = do
    \emitter -> do
      let
        getNext device = do
          emit emitter (HandleDeviceConnection device)
      runSignal $ sig ~> getNext
      pure mempty

  -- Create a Device Event Source from a Device Signal
  deviceEventSource :: Effect (EventSource m Action)
  deviceEventSource = do
    deviceChannel <- createDeviceChannel
    let
      deviceSignal = Chan.subscribe deviceChannel
    pure $ effectEventSource (adaptDeviceSignal deviceSignal)

  adaptMidiEventSignal :: Signal Midi.TimedEvent -> (Emitter Effect Action -> Effect (Finalizer Effect))
  adaptMidiEventSignal sig = do
    \emitter -> do
      let
        getNext timedEvent = do
          emit emitter (HandleMidiEvent timedEvent)
      runSignal $ sig ~> getNext
      pure mempty

  -- Create a Midi-Event Event Source from a Midi-Event Signal
  midiEventSource :: Effect (EventSource m Action)
  midiEventSource = do
    eventChannel <- createEventChannel
    let
      midiEventSignal = Chan.subscribe eventChannel
    pure $ effectEventSource (adaptMidiEventSignal midiEventSignal)
