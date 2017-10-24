module App where


import Data.Midi as Midi
import Data.Midi.Instrument (InstrumentName(..), gleitzmanName)
import Audio.SoundFont (AUDIO, MidiNote, Instrument, loadRemoteSoundFonts, playNote)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff)
import Data.Array (head, null, singleton) as A
import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Data.List (null)
import Data.Tuple (fst)
import Data.Map (Map, insert, delete, empty, isEmpty, values)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Midi.WebMidi (WEBMIDI, Device)
import Data.Midi.Instrument (instruments, read) as MI
import Network.HTTP.Affjax (AJAX)
import Prelude (bind, discard, map, not, pure, ($), (<>), (*), (/), (<<<), (&&), (==))
import Pux (EffModel, noEffects)
import Pux.DOM.Events (onChange, targetValue)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)
import Text.Smolder.HTML (div, h1, p, select, option)
import Text.Smolder.Markup (Attribute, text, (#!), (!))
import Text.Smolder.HTML.Attributes (selected)
import CSS.Geometry (margin)
import CSS.TextAlign (center, textAlign)
import CSS.Size (px, em)
import CSS.Font (fontSize)

-- | volumes in MIDI range from 0 to 127
volumeCeiling :: Int
volumeCeiling = 127

type Devices = Map String Device

data Event
    = NoOp
    | RequestLoadFont InstrumentName
    | FontsLoaded (Array Instrument)
    | DeviceConnection Device
    | ChangeInstrument (Maybe InstrumentName)  -- change the MIDI instrument
    | MidiMessage Midi.TimedEvent              -- a MIDI event message

type State = {
   webMidiConnected :: Boolean
 , inputDevices :: Devices
 , instruments :: Array Instrument
 , maxVolume :: Int                      -- the maximum volume allowed by the volume control
}

initialState :: Boolean -> State
initialState connected = {
  webMidiConnected : connected
, inputDevices : empty
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
          instruments <- loadRemoteSoundFonts $ A.singleton instrumentName
          pure $ Just (FontsLoaded instruments)
      ]
  in
    {state: state, effects: effects}
foldp (FontsLoaded instruments) state =
  noEffects $ state { instruments = instruments }
foldp (DeviceConnection device) state =
  let
    newDevices =
      if device.connected then
        insert device.id device state.inputDevices
      else
        delete device.id state.inputDevices
  in
    noEffects $ state { inputDevices = newDevices }
foldp (ChangeInstrument mInstrumentName ) state =
  case mInstrumentName of
    Just instrumentName ->
      { state: state { instruments = [] }
      , effects: [loadFont instrumentName]
      }
    _ ->
      noEffects state
foldp (MidiMessage timedEvent) state =
  playMidiEvent timedEvent state

loadFont :: ∀ eff. InstrumentName ->  Aff (ajax :: AJAX, au :: AUDIO | eff) (Maybe Event)
loadFont instrumentName =
  do  -- request the fonts are loaded
    instruments <- loadRemoteSoundFonts $ A.singleton instrumentName
    pure $ Just (FontsLoaded instruments)

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

viewInstrument :: Instrument -> HTML Event
viewInstrument instrument =
  do
    (p <<< text)  ((gleitzmanName <<< fst) instrument <> " loaded")

viewInstruments :: State -> HTML Event
viewInstruments state =
  if A.null state.instruments then
    p $ text "wait for instrument to load"
  else
    traverse_ viewInstrument state.instruments

viewDevice :: Device -> HTML Event
viewDevice device =
  do
    p $ text $ device.name <> " " <> device.id

-- | view the connected MIDI input devices
viewInputDevices :: State -> HTML Event
viewInputDevices state =
  if state.webMidiConnected then
    let
      devices = values state.inputDevices
    in
      if null devices then
        do
          p $ text $ "You need to connect a MIDI device"
      else
        do
          traverse_ viewDevice devices
  else
    do
      p $ text ""

-- | display the instrument menu (if we have a connected device)
instrumentMenu :: State -> HTML Event
instrumentMenu state =
  if (state.webMidiConnected && (not isEmpty state.inputDevices)) then
    let
      currentInstrument =
        fromMaybe AcousticGrandPiano $ map (fst) $ A.head state.instruments
    in
      div do
        text "select an instrument"
        select ! selectionStyle #! onChange (\e -> ChangeInstrument (MI.read $ targetValue e) )
          $ (instrumentOptions $ gleitzmanName currentInstrument)
    else
      do
        p $ text ""

-- | build the drop down list of instruments using the gleitz soundfont instrument name
instrumentOptions :: String -> HTML Event
instrumentOptions target =
  let
    f instrument =
        -- option [ selectedInstrument name instrument ]
        if (target == instrument) then
          option ! selected "selected" $ text instrument
        else
          option $ text instrument
  in
    -- this will eventually be renamed as MI.instrumentNames
    traverse_ f MI.instruments


view :: State -> HTML Event
view state =
  div $ do
    h1 ! centreStyle $ text "Midi Keyboard"
    viewInstruments state
    viewInputDevices state
    instrumentMenu state

centreStyle :: Attribute
centreStyle =
  style do
    textAlign center

selectionStyle :: Attribute
selectionStyle  =
  style do
    margin (px 20.0) (px 0.0) (px 0.0) (px 40.0)
    fontSize (em 1.0)
