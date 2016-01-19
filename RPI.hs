module RPI (Program) where

import Control.Lens
import Data.List
import Data.Maybe
import qualified Sound.OSC as OSC

data Program = DimmedLight { _dimmedLightLevel :: Float }

defaultDimmedLightProgram :: DimmedLight
defaultDimmedLightProgram = DimmedLight { _dimmedLightLevel = 0 }

type ProgramName = String
data Message = SwitchProgramMessage { _name :: ProgramName } |
  DimmedLightMessage { _newDimmedLightLevel :: Float } |
  EmptyMessage

data ParsedMessage = Message | Empty

switchProgramAddress = "/rpi/program"
dimLightAddress = "/rpi/dimLight"


type Address = String

applyMessage :: OSC.Message -> Program -> (Program, [OSC.Message])
applyMessage (OSC.Message a datum) program = let message = parseMessage a datum
                                  in case message of
                                     EmptyMessage -> (program, [])
                                     Message m -> (m & setter message .~ fst modified, createMessages m)

parseMessage :: Address -> [OSC.Datum] -> ParsedMessage
parseMessage address datum
  | switchProgramAddress `isPrefixOf` address = SwitchProgramMessage { _name = drop (length switchProgramAddress) address }
  | dimLightAddress `isPrefixOf` address = DimmedLightMessage { _newDimmedLightLevel = firstValue datum }
  | otherwise = EmptyMessage

defaultProgram :: String -> Message

setter :: Message

createMessages :: Message -> [OSC.Message]

-- Utils

firstValue :: OSC.Datem a => [OSC.Datum] -> a
firstValue datum = fromJust $ OSC.d_get $ head datum
