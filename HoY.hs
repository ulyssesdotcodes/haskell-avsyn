{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HoY (HoYProg, applyHoYMessage, defaultHoYProg, hoyProgToMessages) where

import Control.Lens
import Data.Int
import Data.List
import Data.Maybe
import Data.Text.Lens
import qualified Data.Text as T
import qualified Sound.OSC as OSC

data HoYProg = HoYProg { _program :: String
                       , _programs :: [String]
                       , _cues :: [ProgSettings]
                       }

data ProgSettings = ProgSettings { _progName :: String
                                 , _progPath :: String
                                 , _progControls :: [Control]
                                 }

defaultHoYProg :: HoYProg
defaultHoYProg = HoYProg { _program = "Blank"
                         , _programs = ["Blank", "Nightlife", "Fire", "Smoke", "BlueLight"]
                         , _cues = [ ProgSettings { _progName = "Nightlife"
                                                  , _progPath = "nightlife"
                                                  , _progControls =
                                                    [ Control "startx" (ControlSlider $ Slider 0 (-3) 3)
                                                    , Control "starty" (ControlSlider $ Slider (-1.3) (-2) 1)
                                                    , Control "midx" (ControlSlider $ Slider (-1) (-3) 3)
                                                    , Control "midy" (ControlSlider $ Slider (-1.3) (-2) 1)
                                                    , Control "endx" (ControlSlider $ Slider (-1) (-3) 3)
                                                    , Control "endy" (ControlSlider $ Slider (-1.3) (-2) 1)
                                                    , Control "reset" (ControlToggle $ Toggle 0)
                                                    , Control "animate" (ControlToggle $ Toggle 1)
                                                    , Control "end" (ControlToggle $ Toggle 0)
                                                    ]
                                                  }
                                   , ProgSettings { _progName = "Smoke"
                                                  , _progPath = "smoke"
                                                  , _progControls =
                                                    [ Control "darkness" (ControlSlider $ Slider 0 0 1)
                                                    ]}
                                   , ProgSettings { _progName = "Fire"
                                                   , _progPath = "fire"
                                                   , _progControls =
                                                    [ Control "intensity" (ControlSlider $ Slider 1 0.85 1.15)
                                                    ]}
                                   , ProgSettings { _progName = "BlueLight"
                                                   , _progPath = "bluelight"
                                                   , _progControls =
                                                    [ Control "intensity" (ControlSlider $ Slider 1 0.0 2)
                                                    , Control "size" (ControlSlider $ Slider 0.002 0 0.012)
                                                    , Control "x" (ControlSlider $ Slider 0.5 0 1)
                                                    , Control "y" (ControlSlider $ Slider 0.2 0 1)
                                                    ]}
                                   ]
                         }


data Slider = Slider { _controlSliderValue :: Float
                     , _controlSliderMin :: Float
                     , _controlSliderMax :: Float
                     } deriving (Show)

data Toggle = Toggle {  _controlToggleValue :: Float
                     } deriving (Show)

data ControlData = ControlSlider Slider | ControlToggle Toggle

data Control = Control { _controlName :: String
                       , _controlData :: ControlData
                       }


data Message = ProgramMessage { _newProgram :: String } |
  ProgSettingsSliderMessage { _progSettingsProgPath :: String
                            , _progSettingsSliderPath :: String
                            , _progSettingsMsgVal :: Float } |
  EmptyMessage

baseAddress = "/hoy/"
cuesAddress = "/hoy/cues/"
programAddress = "/hoy/program"

-- Lenses

makeLenses ''HoYProg
makeLenses ''ProgSettings
makeLenses ''Slider
makeLenses ''Toggle
makeLenses ''Control
makePrisms ''ControlData
makePrisms ''Message

-- OSC Flags

controlFlag = OSC.d_put (32 :: Int32)
toggleFlag = OSC.d_put $ OSC.ascii "b"
sliderFlag = OSC.d_put $ OSC.ascii "f"


-- The good shit

hoyProgToMessages :: HoYProg -> [OSC.Message]
hoyProgToMessages hoyProg = [OSC.Message "/hoy/programs" (map (OSC.d_put . OSC.ascii) (hoyProg ^. programs))] ++
                            [OSC.Message "/hoy/program" [(OSC.d_put . OSC.ascii) (hoyProg ^.program)]] ++
                            (concatMap (progSettingToMessages True) (hoyProg ^. cues))

progSettingToMessages :: Bool -> ProgSettings -> [OSC.Message]
progSettingToMessages isClient progSetting =
  map (controlToMessage isClient address) (progSetting ^. progControls)
  where
    address = cuesAddress ++ (progSetting ^. progPath)

-- Create a message of val (maybe with whole control) address "/hoy/cues/[controlName]"
controlToMessage :: Bool -> String -> Control -> OSC.Message
controlToMessage isClient path control = OSC.Message (path ++ "/" ++ control ^. controlName) (controlToDatum control isClient)

applyHoYMessage :: OSC.Message -> HoYProg -> (HoYProg, [OSC.Message])
applyHoYMessage (OSC.Message a datum) program = let message = parseMessage a datum
                                  in (setter message program, createMessages message)

parseMessage :: String -> [OSC.Datum] -> Message
parseMessage address datum
  | programAddress `isPrefixOf` address =
    ProgramMessage { _newProgram = OSC.ascii_to_string $ firstValue datum }
  | cuesAddress `isPrefixOf` address = parseProgSettingsMessage (drop (length cuesAddress) address) datum
  | otherwise = EmptyMessage

parseProgSettingsMessage :: String -> [OSC.Datum] -> Message
parseProgSettingsMessage address datum =
  ProgSettingsSliderMessage (head pathSegments) (pathSegments!!(length pathSegments - 2)) (fromJust $ OSC.d_get (head datum))
  where
    pathSegments = map T.unpack $ T.splitOn "/" (T.pack address)

setter :: Message -> HoYProg -> HoYProg
setter ProgramMessage { _newProgram=program_ } = set program program_
setter ProgSettingsSliderMessage { _progSettingsProgPath = path_
                                 , _progSettingsSliderPath = sliderPath_
                                 , _progSettingsMsgVal = value_}
  = over (cues . traverse) (changeCueSliderControl path_ sliderPath_ value_)
setter EmptyMessage = programId

changeCueSliderControl :: String -> String -> Float -> ProgSettings -> ProgSettings
changeCueSliderControl path sliderPath value cue
  | path == (cue ^. progPath) = over (progControls . traverse) (changeSliderControl sliderPath value) cue
  | otherwise = cue

changeSliderControl :: String -> Float -> Control -> Control
changeSliderControl sliderPath value control
  | sliderPath == (control ^. controlName) = set (controlData . _ControlSlider . controlSliderValue) value control
  | otherwise = control

programId :: HoYProg -> HoYProg
programId program = program

createMessages :: Message -> [OSC.Message]
createMessages ProgramMessage { _newProgram=program_ } =
  [OSC.Message "/hoy/program" [OSC.d_put (OSC.ascii program_)]]
createMessages ProgSettingsSliderMessage { _progSettingsProgPath = path_
                                         , _progSettingsSliderPath = sliderPath_
                                         , _progSettingsMsgVal = value_}
  = [OSC.Message (cuesAddress ++ path_ ++ "/" ++ sliderPath_ ++ "/value") [OSC.d_put value_]]
createMessages EmptyMessage = []

-- Utils

firstValue :: OSC.Datem a => [OSC.Datum] -> a
firstValue datum = fromJust $ OSC.d_get $ head datum

controlToDatum :: Control -> Bool -> [OSC.Datum]
controlToDatum Control {_controlName = name, _controlData = control} isClient = [controlFlag, (OSC.d_put . OSC.ascii) name] ++ (controlValueToDatum control isClient)

controlValueToDatum :: ControlData -> Bool -> [OSC.Datum]
controlValueToDatum (ControlSlider control) isClient
   | isClient = [ sliderFlag
                , toDatum controlSliderValue
                , toDatum controlSliderMin
                , toDatum controlSliderMax
                ]
   | otherwise = [toDatum controlSliderValue]
   where
     toDatum f = OSC.d_put $ view f control

controlValueToDatum (ControlToggle control) isClient
   | isClient = [ toggleFlag
               , OSC.d_put $ view controlToggleValue control
               ]
   | otherwise  = [OSC.d_put $ view controlToggleValue control]
