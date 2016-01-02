{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Cinder (Mixer, newCinderState, applyMessage, mixerToMessages) where

import Control.Lens
import Data.Data
import Data.Either
import Data.Int
import Data.List
import Data.Maybe
import Data.Typeable
import GHC.Float
import qualified Data.ByteString.Char8 as BSC
import qualified Sound.OSC as OSC

data Mixer = Mixer { _mixControls :: [Control]
                   , _mixChoiceA :: ChoiceVis
                   , _mixChoiceB :: ChoiceVis
                   , _mixVisualizations :: [Visualization]
                   } deriving (Show)

data ChoiceVis = ChoiceVis { _choiceVisualization :: Int32
                           , _choiceControls :: [Control] } deriving (Show)

data Visualization = Visualization { _visName :: String
                                   , _visControls :: [Control]
                                   } deriving (Show)

data Slider = Slider { _controlSliderName :: String
                     , _controlSliderValue :: Float
                     , _controlSliderMin :: Float
                     , _controlSliderMax :: Float
                     } deriving (Show)

data Toggle = Toggle { _controlToggleName :: String
                     , _controlToggleValue :: Float
                     } deriving (Show)

data Control = ControlSlider Slider | ControlToggle Toggle  deriving (Show)

defaultChoiceVis :: ChoiceVis
defaultChoiceVis = ChoiceVis { _choiceVisualization = 1
                             , _choiceControls =
                                [ ControlToggle $ Toggle "Apply Effects" 0
                                , ControlToggle $ Toggle "Fade Transition" 0
                                , ControlSlider $ Slider "Fade" 0 0 1
                                , ControlSlider $ Slider "Effect Fade" 0 0 1
                                , ControlSlider $ Slider "Scale" 1 0.85 1.15
                                , ControlSlider $ Slider "Offset Y" 0 ( -0.15 ) 0.15
                                , ControlSlider $ Slider "Hue Shift" 0 0 1
                                , ControlSlider $ Slider "Hue Shift Cycle" 0 0 0.25
                                , ControlSlider $ Slider "Saturation Shift" 0 0 1
                                , ControlSlider $ Slider "Lightness Shift" 1 0 2
                                ] }

defaultVisualizations :: [Visualization]
defaultVisualizations = [ Visualization { _visName="Blank", _visControls=[]}
                        , Visualization { _visName="Buffer"
                                        , _visControls=[ ControlSlider $  Slider "Volume" 1 0 2]}
                        ]

newCinderState :: Mixer
newCinderState = Mixer {  _mixControls=[ ControlSlider $ Slider "Fade" 0.5 0 1
                                       , ControlSlider $ Slider "Add" 1 0 2
                                       , ControlSlider $ Slider "Multiply" 0 0 6
                                       ]
                       , _mixChoiceA=defaultChoiceVis
                       , _mixChoiceB=defaultChoiceVis
                       , _mixVisualizations=defaultVisualizations
                       }


-- Lenses

makeLenses ''Mixer
makeLenses ''ChoiceVis
makeLenses ''Toggle
makeLenses ''Slider
makeLenses ''Visualization
makePrisms ''Control


-- Address definition

mixControlsAddress = "/mix/controls/" :: String
visAEffectsAddress = "/visA/effects/" :: String
visBEffectsAddress = "/visB/effects/" :: String
visASlidersAddress = "/visA/sliders/" :: String
visBSlidersAddress = "/visB/sliders/" :: String
visAChoiceAddress = "/visA/choice"
visBChoiceAddress = "/visB/choice"
visesAddress = "/vises/" :: String
choicesAddress = "/choices" :: String

-- OSC Flags

controlFlag = OSC.d_put (32 :: Int)
toggleFlag = OSC.d_put $ BSC.pack "b"
sliderFlag = OSC.d_put $ BSC.pack "f"

-- Applying messages

-- modifyControlList path controlName datum controls = (mapControls, createMessages)
applyMessage :: OSC.Message -> Mixer -> (Mixer, [OSC.Message])
applyMessage (OSC.Message address datum) mixer
  | mixControlsAddress `isPrefixOf` address = createResult (mixControls) modifiedMixControls
  | visAEffectsAddress `isPrefixOf` address = createResult (mixChoiceA . choiceControls) modifiedChoiceAControl
  | visBEffectsAddress `isPrefixOf` address = createResult (mixChoiceA . choiceControls) modifiedChoiceBControl
  | visAChoiceAddress `isPrefixOf` address = (mixer & mixChoiceA . choiceVisualization .~ ((fromIntegral . float2Int) (firstValue datum)), [OSC.Message visAChoiceAddress [OSC.d_put $ (((fromIntegral . float2Int) (firstValue datum)) :: Int32)]])
  | visBChoiceAddress `isPrefixOf` address = (mixer & mixChoiceA . choiceVisualization .~ ((fromIntegral . float2Int) (firstValue datum)), [OSC.Message visBChoiceAddress [OSC.d_put $ (((fromIntegral . float2Int) (firstValue datum)) :: Int32)]])
  | otherwise = (mixer, [])
  where
    extractNewAddress pattern = drop (length pattern) address
    modifiedControls path getter =
      modifyControlList path (extractNewAddress path) datum (view getter mixer)
    modifiedMixControls = modifiedControls mixControlsAddress mixControls
    modifiedChoiceAControl = modifiedControls visAEffectsAddress (mixChoiceA . choiceControls)
    modifiedChoiceBControl = modifiedControls visBEffectsAddress (mixChoiceB . choiceControls)
    createResult getter modifier = (set getter (fst modifier) mixer, snd modifier)

modifyControlList :: String -> String -> [OSC.Datum] -> [Control] -> ([Control], [OSC.Message])
modifyControlList path controlName datum controls = (mapControls, createMessages)
  where
    createMessages = [OSC.Message ( path ++ controlName ) [(head datum)]]
    firstDatumValue = firstValue datum
    mapControls = controls & traverse %~ (mapControl controlName firstDatumValue)

firstValue :: OSC.Datem a => [OSC.Datum] -> a
firstValue datum = fromJust $ OSC.d_get $ head datum

valueMessages :: String -> OSC.Datum -> [OSC.Message]
valueMessages fullAddress datum = [OSC.Message fullAddress [datum]]

mixerToMessages :: Bool -> Mixer -> [OSC.Message]
mixerToMessages isClient mixer =
  (createMessages mixControls mixControlsAddress) ++
  (createMessages (mixChoiceA . choiceControls) visAEffectsAddress) ++
  (createMessages (mixChoiceB . choiceControls) visBEffectsAddress) ++
  ([createChoiceVisChoiceMessage visAChoiceAddress mixChoiceA]) ++
  ([createChoiceVisChoiceMessage visBChoiceAddress mixChoiceB]) ++
  ([createChoicesMessage])
  where
    createMessages getter path = map (createMessage path) $ view getter mixer
    createMessage path (ControlSlider control) =
      OSC.Message (path ++ view controlSliderName control) (controlSliderToDatum isClient control)
    createMessage path (ControlToggle control) =
      OSC.Message (path ++ view controlToggleName control) (controlToggleToDatum isClient control)
    createChoicesMessage = OSC.Message choicesAddress $ map (OSC.d_put . BSC.pack . (view visName)) $ mixer ^. mixVisualizations
    createChoiceVisChoiceMessage address getter =
        OSC.Message address $ [OSC.d_put (mixer ^. getter . choiceVisualization)]

mapControl :: String -> Float -> Control -> Control
mapControl controlName value control
      | (matches _ControlSlider) && sliderHasName control = mapValue $ _ControlSlider . controlSliderValue
      | (matches _ControlToggle) && toggleHasName control = mapValue $ _ControlToggle . controlToggleValue
      | otherwise = control
  where
    sliderHasName slider = (controlName == slider ^. _ControlSlider . controlSliderName)
    toggleHasName toggle = (controlName == toggle ^. _ControlToggle . controlToggleName)
    matches f = isRight (matching f control)
    mapValue f = control & f .~ value

controlSliderToDatum :: Bool -> Slider -> [OSC.Datum]
controlSliderToDatum isClient control
  | isClient == True = [ controlFlag
                       , sliderFlag
                       , OSC.d_put $ BSC.pack $ view controlSliderName control
                       , toDatum controlSliderValue
                       , toDatum controlSliderMin
                       , toDatum controlSliderMax
                       ]
  | otherwise = [toDatum controlSliderValue]
  where
    toDatum f = OSC.d_put $ view f control

controlToggleToDatum :: Bool -> Toggle -> [OSC.Datum]
controlToggleToDatum isClient control
  | isClient == True = [ controlFlag
                       , toggleFlag
                       , OSC.d_put $ BSC.pack $ view controlToggleName control
                       , OSC.d_put $ view controlToggleValue control
                       ]
  | otherwise  = [OSC.d_put $ view controlToggleValue control]
