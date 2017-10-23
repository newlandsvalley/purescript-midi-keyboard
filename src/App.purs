module App where


import Data.Midi as Midi
import Audio.SoundFont (AUDIO, MidiNote, Instrument, loadRemoteSoundFonts, playNote)
import CSS.TextAlign (textAlign, leftTextAlign, center)
import Control.Bind ((=<<))
import Control.Monad.Eff.Class (liftEff)
import Data.Array (length, fromFoldable, slice, singleton)
import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Data.List (List(..), null)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Midi.Instrument (InstrumentName(..), gleitzmanName, instruments, read)
import Data.Midi.WebMidi (WEBMIDI, Device)
import Network.HTTP.Affjax (AJAX)
import Prelude (bind, const, discard, map, max, min, pure, show, ($), (#), (<>), (+), (-), (*), (/), (==), (<<<))
import Pux (EffModel, noEffects)
import Pux.DOM.Events (onClick, onChange, onInput, targetValue)
import Pux.DOM.HTML (HTML, child)
import Pux.DOM.HTML.Attributes (style)
import Text.Smolder.HTML (button, div, h1, input, label, p, span, textarea, ul, li)
import Text.Smolder.Markup (attribute, text, (#!), (!))
import Text.Smolder.Markup (Attribute)

-- | volumes in MIDI range from 0 to 127
volumeCeiling :: Int
volumeCeiling = 127

data Event
    = NoOp
    | RequestLoadFont InstrumentName
    | FontsLoaded (Array Instrument)
    | DeviceConnection Device
    | MidiMessage Midi.TimedEvent    -- a MIDI event message

type State = {
   inputDevices :: Array Device
 , instruments :: Array Instrument
 , maxVolume :: Int                      -- the maximum volume allowed by the volume control
}

initialState :: State
initialState = {
  inputDevices : []
, instruments : []
, maxVolume : (volumeCeiling / 2)  -- start at half volume ceiling
}

foldp :: Event -> State -> EffModel State Event (ajax :: AJAX, wm :: WEBMIDI, au :: AUDIO)
foldp NoOp state =  noEffects $ state
foldp (RequestLoadFont instrumentName) state =
  let
    effects =
      [
        do  -- request the fonts are loaded
          instruments <- loadRemoteSoundFonts $ singleton instrumentName
          pure $ Just (FontsLoaded instruments)
      ]
  in
    {state: state, effects: effects}
foldp (FontsLoaded instruments) state =
  noEffects $ state { instruments = instruments }
foldp (DeviceConnection device) state =
  let
    newState = state { inputDevices = singleton device}
  in
    noEffects newState
foldp (MidiMessage timedEvent) state =
  playMidiEvent timedEvent state

-- | interpret MIDI event messages
-- | at the moment we only respond to:
-- |    NoteOn
-- |    Control Volume
-- |
-- | but obviously this is easily extended to other messages
-- | Also, volume control discriminate neither which
-- | device is being played nor which MIDI channel is in operation
-- | (i.e. you're probably OK if you just attach a single device)
playMidiEvent :: ∀ eff. Midi.TimedEvent -> State -> EffModel State Event (au :: AUDIO, wm :: WEBMIDI | eff )
playMidiEvent (Midi.TimedEvent te) state =
    let
      maybeEvent = te.event
    in
      case maybeEvent of
        Just (Midi.NoteOn channel pitch velocity) ->
          { state : state
          , effects :
            [ do
                let
                  -- respond to the current volume control setting
                  volumeScale =
                    toNumber state.maxVolume / toNumber volumeCeiling
                  -- and this is what's left of the note
                  gain =
                    toNumber velocity * volumeScale / toNumber volumeCeiling
                  midiNote :: MidiNote
                  midiNote = { channel : channel, id: pitch, timeOffset: 0.0, duration : 1.0, gain : gain }
                _ <- liftEff $ playNote state.instruments midiNote
                pure $ Just NoOp
            ]
          }
        _ ->
          let
            newState = recogniseControlMessage maybeEvent state
          in
            noEffects newState

-- | recognise and act on a control message and save to the model state
-- |    At the moment, we just recognise volume changes
recogniseControlMessage :: Maybe Midi.Event -> State -> State
recogniseControlMessage mevent state =
  case mevent of
    Just (Midi.ControlChange channel 7 amount) ->
      state { maxVolume = amount }
    _ ->
      state


showDevice :: Device -> HTML Event
showDevice device =
  do
    p $ text $ device.name <> " " <> device.id

-- | view the connected MIDI input devices
viewInputDevices :: State -> HTML Event
viewInputDevices state =
  -- case state.webMidiConnection of
  --  Connected ->
      case state.inputDevices of
        [] ->
          do
            p $ text $ "You need to connect a MIDI device"
        _ ->
          do
            traverse_ showDevice state.inputDevices
    -- _ ->
    --  do
    --    p $ text ""


view :: State -> HTML Event
view state =
  div $ do
    h1 ! centreStyle $ text "Midi Keyboard"
    viewInputDevices state


centreStyle :: Attribute
centreStyle =
  style do
    textAlign center
