{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Cinder (Mixer, newCinderState, applyMessage, mixerToMessages) where

import Control.Lens
import Data.Data
import Data.List
import Data.Maybe
import Data.Typeable
import qualified Data.ByteString.Char8 as BSC
import qualified Sound.OSC as OSC

data Mixer = Mixer { _mixFade :: ControlSlider
                   , _mixAdd :: ControlSlider
                   , _mixMultiply :: ControlSlider
                   , _mixChoiceA :: ChoiceVis
                   , _mixChoiceB :: ChoiceVis
                   }

data ChoiceVis = ChoiceVis { _choiceApplyEffects :: ControlToggle
                           , _choiceFadeTransition :: ControlToggle
                           , _choiceFade :: ControlSlider
                           , _choiceEffectFade :: ControlSlider
                           , _choiceScale :: ControlSlider
                           , _choiceOffsetY :: ControlSlider
                           , _choiceHueShift :: ControlSlider
                           , _choiceHueShiftCycle :: ControlSlider
                           , _choiceSaturationShift :: ControlSlider
                           , _choiceLightnessShift :: ControlSlider
                           }

-- data Visualization = Visualization { visName :: String
--                                    , visControls :: [AnyControl]
--                                    }

data ControlSlider = ControlSlider { _controlSliderName :: String
                                   , _controlSliderValue :: Float
                                   , _controlSliderMin :: Float
                                   , _controlSliderMax :: Float
                                   }

data ControlToggle = ControlToggle { _controlToggleName :: String
                                   , _controlToggleValue :: Float
                                   }

data AnyControl = AnyControlSlider ControlSlider | AnyControlToggle ControlToggle

defaultChoiceVis :: ChoiceVis
defaultChoiceVis = ChoiceVis { _choiceApplyEffects=ControlToggle "Apply Effects" 0
                             , _choiceFadeTransition=ControlToggle "Fade Transition" 0
                             , _choiceFade=ControlSlider "Fade" 0 0 1
                             , _choiceEffectFade=ControlSlider "Effect Fade" 0 0 1
                             , _choiceScale=ControlSlider "Scale" 0 (-0.15) 0.15
                             , _choiceOffsetY=ControlSlider "Offset Y" 0 ( -0.15 ) 0.15
                             , _choiceHueShift=ControlSlider "Hue Shift" 0 0 1
                             , _choiceHueShiftCycle=ControlSlider "Hue Shift Cycle" 0 0 0.25
                             , _choiceSaturationShift=ControlSlider "Saturation Shift" 0 0 1
                             , _choiceLightnessShift=ControlSlider "Lightness Shift" 0 0 1
                             }

newCinderState :: Mixer
newCinderState = Mixer { _mixFade=ControlSlider "Fade" 0.5 0 1
                       , _mixAdd=ControlSlider "Add" 1 0 2
                       , _mixMultiply=ControlSlider "Multiply" 0 0 6
                       , _mixChoiceA=defaultChoiceVis
                       , _mixChoiceB=defaultChoiceVis
                       }


-- Lenses

makeLenses ''Mixer
makeLenses ''ChoiceVis
makeLenses ''ControlToggle
makeLenses ''ControlSlider
makePrisms ''AnyControl


-- Address definitions

mixControls = "/mix/controls/"

-- OSC Flags

controlFlag = OSC.d_put (32 :: Int)
toggleFlag = OSC.d_put $ BSC.pack "b"
sliderFlag = OSC.d_put $ BSC.pack "f"

-- Applying messages

applyMessage :: OSC.Message -> Mixer -> (Mixer, [OSC.Message])
applyMessage (OSC.Message address datum) mixer
  | mixControls `isPrefixOf` address =
    applyMixControlMessage (extractNewAddress mixControls) datum mixer
  | visAEffects `isPrefixOf` address =
    let (choice, messages) = applyChoiceEffectsMessage visAEffects (extractNewAddress visAEffects) datum (view mixChoiceA mixer)
    in (set mixChoiceA choice mixer, messages)
  | otherwise = (mixer, [])
  where
    extractNewAddress pattern = drop (length mixControls) address
    visAEffects = "/visA/effects/"

applyMixControlMessage :: String -> [OSC.Datum] -> Mixer -> (Mixer, [OSC.Message])
applyMixControlMessage address datum mixer
  | "Fade" `isPrefixOf` address = createResult mixFade
  | "Add" `isPrefixOf` address = createResult mixAdd
  | "Multiply" `isPrefixOf` address = createResult mixMultiply
  where
    fullAddress = mixControls ++ address
    firstDatumValue = firstValue datum
    createMessages = valueMessages fullAddress (head datum)
    setValue f = set (f . controlSliderValue) firstDatumValue mixer
    createResult f = (setValue f, createMessages)

applyChoiceEffectsMessage :: String -> String -> [OSC.Datum] -> ChoiceVis -> (ChoiceVis, [OSC.Message])
applyChoiceEffectsMessage path controlName datum choice
  | "Apply Effects" `isPrefixOf` controlName = (set (choiceApplyEffects . controlToggleValue) (firstValue datum) choice, createMessages)
  | "Fade" `isPrefixOf` controlName = (set (choiceFade . controlSliderValue) (firstValue datum) choice, createMessages)
  | otherwise = (choice, [])
  where
    createMessages = [OSC.Message (path ++ controlName) [(head datum)]]

firstValue :: OSC.Datem a => [OSC.Datum] -> a
firstValue datum = fromJust $ OSC.d_get $ head datum

valueMessages :: String -> OSC.Datum -> [OSC.Message]
valueMessages fullAddress datum = [OSC.Message fullAddress [datum]]

mixerToMessages :: Mixer -> [OSC.Message]
mixerToMessages mixer = (createMixMessages mixer) ++
  (createChoiceMessages (view mixChoiceA mixer) "/visA") ++
  (createChoiceMessages (view mixChoiceB mixer) "/visB")

createMixMessages :: Mixer -> [OSC.Message]
createMixMessages mixer = [ createMessage (address mixFade) mixFade
                          , createMessage (address mixAdd) mixAdd
                          , createMessage (address mixMultiply) mixMultiply
                          ]
  where
    address f = mixControls ++ view (f . controlSliderName) mixer
    viewControl f = view f mixer
    createMessage addr f = OSC.Message addr (controlSliderToDatum $ viewControl f)

createChoiceMessages :: ChoiceVis -> String -> [OSC.Message]
createChoiceMessages vis address = [ OSC.Message (toggleAddr $ view choiceApplyEffects vis) (controlToggleToDatum $ viewControl choiceApplyEffects)
                                   , OSC.Message (sliderAddr $ view choiceFade vis) (controlSliderToDatum $ viewControl choiceFade)
                                   ]
  where
    viewControl f = view f vis
    sliderAddr (ControlSlider name __ ___ ____) = effectsAddr name
    toggleAddr (ControlToggle name __ ) = effectsAddr name
    effectsAddr a = address ++ "/effects/" ++ a

controlSliderToDatum :: ControlSlider -> [OSC.Datum]
controlSliderToDatum control = [ controlFlag
                         , sliderFlag
                         , OSC.d_put $ BSC.pack $ view controlSliderName control
                         , toDatum controlSliderValue
                         , toDatum controlSliderMin
                         , toDatum controlSliderMax
                         ]
  where
    toDatum f = OSC.d_put $ view f control

controlToggleToDatum :: ControlToggle -> [OSC.Datum]
controlToggleToDatum control = [ controlFlag
                         , toggleFlag
                         , OSC.d_put $ BSC.pack $ view controlToggleName control
                         , OSC.d_put $ view controlToggleValue control
                         ]
  where
    anyControlToggle = AnyControlToggle control

