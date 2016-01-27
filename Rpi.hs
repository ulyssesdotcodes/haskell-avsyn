{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Rpi (Program, applyRpiMessage, defaultProgram) where

import Control.Lens
import Data.Int
import Data.List
import Data.Maybe
import Data.Text.Lens
import qualified Sound.OSC as OSC

data Program = Program { _lightLevel:: Float
                       , _program :: String
                       }

defaultProgram :: Program
defaultProgram = Program { _lightLevel = 0
                         , _program = "lit"
                         }


data Message = ProgramMessage { _newProgram :: String } |
  LightLevelMessage { _newLightLevel :: Float } |
  EmptyMessage

programAddress = "/rpi/program"
lightLevelAddress = "/rpi/lightLevel"

-- Lenses

makeLenses ''Program
makePrisms ''Message

applyRpiMessage :: OSC.Message -> Program -> (Program, [OSC.Message])
applyRpiMessage (OSC.Message a datum) program = let message = parseMessage a datum
                                  in (setter message program, createMessages message)

parseMessage :: String -> [OSC.Datum] -> Message
parseMessage address datum
  | programAddress `isPrefixOf` address =
    ProgramMessage { _newProgram = OSC.ascii_to_string $ firstValue datum }
  | lightLevelAddress `isPrefixOf` address = LightLevelMessage { _newLightLevel = firstValue datum }
  | otherwise = EmptyMessage

setter :: Message -> Program -> Program
setter ProgramMessage { _newProgram=program_ } = set program program_
setter LightLevelMessage { _newLightLevel=lightLevel_ } = set lightLevel lightLevel_
setter EmptyMessage = programId

programId :: Program -> Program
programId program = program

createMessages :: Message -> [OSC.Message]
createMessages ProgramMessage { _newProgram=program_ } =
  [OSC.Message "/rpi/program" [OSC.d_put (OSC.ascii program_)]]
createMessages LightLevelMessage { _newLightLevel=lightLevel_ } =
  [OSC.Message "/rpi/lightLevel" [OSC.d_put lightLevel_]]
createMessages EmptyMessage = []

-- Utils

firstValue :: OSC.Datem a => [OSC.Datum] -> a
firstValue datum = fromJust $ OSC.d_get $ head datum
