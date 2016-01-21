k{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Rpi (Program, applyRpiMessage, defaultRpi) where

import Control.Lens
import Data.Int
import Data.List
import Data.Maybe
import qualified Sound.OSC as OSC

data Program = DimmedLight { _dimmedLightLevel :: Float }

defaultDimmedLightProgram :: Program
defaultDimmedLightProgram = DimmedLight { _dimmedLightLevel = 0 }

type ProgramName = String
data Message = SwitchProgramMessage { _name :: ProgramName } |
  DimmedLightMessage { _newDimmedLightLevel :: Float } |
  EmptyMessage

data ParsedMessage = Message | Empty

switchProgramAddress = "/rpi/program"
dimLightAddress = "/rpi/dimLight"

defaultRpi = defaultDimmedLightProgram

-- Lenses

makePrisms ''Program

applyRpiMessage :: OSC.Message -> Program -> (Program, [OSC.Message])
applyRpiMessage (OSC.Message a datum) program = let message = parseMessage a datum
                                  in case message of
                                     EmptyMessage -> (program, [])
                                     Message m -> (m & setter message .~ fst modified, createMessages m)

parseMessage :: String -> [OSC.Datum] -> ParsedMessage
parseMessage address datum
  | switchProgramAddress `isPrefixOf` address = SwitchProgramMessage { _name = drop (length switchProgramAddress) address }
  | dimLightAddress `isPrefixOf` address = DimmedLightMessage { _newDimmedLightLevel = firstValue datum }
  | otherwise = EmptyMessage

setter :: OSC.Message -> Program -> Program
setter DimmedLight { _dimmedLightLevel=level } p = set p 

createMessages :: Message -> [OSC.Message]
createMessages DimmedLight { _dimmedLightLevel=level } =
  [OSC.Message "/rpi/program" [OSC.d_put (1 :: Int32)], OSC.Message "/rpi/dimLight" [OSC.d_put level]]

-- Utils

firstValue :: OSC.Datem a => [OSC.Datum] -> a
firstValue datum = fromJust $ OSC.d_get $ head datum
