module Render where

-- Rendering functions
import Prelude (($), (<<<), (<>), (==), map)
import State (ChildSlots, State, Query(..))
import Halogen.HTML.Core (HTML)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Data.Array (cons, head, null)
import Data.Map (Map, isEmpty, values)
import Data.Tuple (fst)
import Data.Maybe (fromMaybe)
import Data.List (toUnfoldable)
import Audio.SoundFont (Instrument)
import Data.Midi.Instrument (InstrumentName(..), gleitzmanName, instrumentNames, read)
import Data.Midi.WebMidi (Device)

renderConnection :: ∀ a b.
  State
  -> HTML a b
renderConnection state =
  if (state.webMidiConnected) then
    HH.div
      [ HP.class_ (H.ClassName "leftPanelComponent")]
      [ renderDevices state.inputDevices ]
  else
    HH.div
      [ HP.class_ (H.ClassName "leftPanelComponent")]
      [HH.text "Perhaps web-MIDI is not supported in this browser"]

renderInstruments :: ∀ a b.
  Array Instrument
  -> HTML a b
renderInstruments instruments =
  if (null instruments) then
    HH.div_ [ HH.text "wait for instruments to load"]
  else
    HH.div
      [ HP.class_ (H.ClassName "leftPanelComponent") ]
      [ HH.div
         [ HP.class_ (H.ClassName "longLabel") ]
         [ HH.text "loaded instruments:" ]
      , HH.ul
        [ HP.class_ $ H.ClassName "msListItem" ]
        $ map renderInstrument instruments
      ]

renderInstrument :: ∀ a b.
  Instrument
  -> HTML a b
renderInstrument instrument =
  HH.li
    [ HP.class_ $ H.ClassName "msListItemLabel" ]
    [ HH.text $ (gleitzmanName <<< fst) instrument ]

renderDevices :: ∀ a b.
    Map String Device
  -> HTML a b
renderDevices devices =
  if (isEmpty devices) then
    HH.div_ [ HH.text "You need to connect a MIDI input device such as a keyboard"]
  else
    HH.div
      [ HP.class_ (H.ClassName "leftPanelComponent") ]
      [ HH.div
         [ HP.class_ (H.ClassName "longLabel") ]
         [ HH.text "attached devices:" ]
      , HH.ul
         [ HP.class_ $ H.ClassName "msListItem" ]
         $ map renderDevice $ (toUnfoldable <<< values) devices
      ]

renderDevice :: ∀ a b.
    Device
  -> HTML a b
renderDevice device =
  HH.li
    [ HP.class_ (H.ClassName "msListItemLabel") ]
    [ HH.text $ device.name <> " " <> device.id ]

renderInstrumentMenu :: ∀ m. Array Instrument -> H.ComponentHTML Query ChildSlots m
renderInstrumentMenu instruments =
  let
    currentInstrument =
      fromMaybe AcousticGrandPiano $ map (fst) $ head instruments
  in
    HH.div
      [ HP.class_ (H.ClassName "leftPanelComponent")]
      [ HH.label
         [ HP.class_ (H.ClassName "labelAlignment") ]
         [ HH.text "change instrument: " ]
      , HH.select
          [ HP.class_ $ H.ClassName "selection"
          , HP.id_  "instrument-menu"
          , HP.value (gleitzmanName currentInstrument)
          , HE.onValueChange  (HE.input (\i -> HandleChangeInstrument $ read i))
          ]
          (cons
            (HH.option [ ] [ HH.text (gleitzmanName currentInstrument)])
            (instrumentOptions currentInstrument)
          )
      ]

instrumentOptions :: ∀ a b. InstrumentName -> Array (HTML a b)
instrumentOptions currentInstrument =
  let
    f :: ∀ p i. InstrumentName -> HTML p i
    f inst =
      let
         disabled = (inst == currentInstrument)
      in
        HH.option
          [ HP.disabled disabled ]
          [ HH.text (gleitzmanName inst)]
  in
    map f (toUnfoldable instrumentNames)
