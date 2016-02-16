{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HoY (HoYProg, applyHoYMessage, defaultHoYProg) where

import Control.Lens
import Data.Int
import Data.List
import Data.Maybe
import Data.Text.Lens
import qualified Sound.OSC as OSC

data HoYProg = HoYProg { _program :: String
                       , _programs :: [String]
                       }

defaultHoYProg :: HoYProg
defaultHoYProg = HoYProg { _program = "Blank",
                           _programs = ["Blank", "Nightlife"]
                         }


data Message = ProgramMessage { _newProgram :: String } |
  EmptyMessage

programAddress = "/hoy/program"

-- Lenses

makeLenses ''HoYProg
makePrisms ''Message

hoyProgToMessages :: HoYProg -> [OSC.Message]
hoyProgToMessages hoyProg = [OSC.Message "/hoy/programs" (map (OSC.d_put . OSC.ascii) (hoyProg ^. programs))]

applyHoYMessage :: OSC.Message -> HoYProg -> (HoYProg, [OSC.Message])
applyHoYMessage (OSC.Message a datum) program = let message = parseMessage a datum
                                  in (setter message program, createMessages message)

parseMessage :: String -> [OSC.Datum] -> Message
parseMessage address datum
  | programAddress `isPrefixOf` address =
    ProgramMessage { _newProgram = OSC.ascii_to_string $ firstValue datum }
  | otherwise = EmptyMessage

setter :: Message -> HoYProg -> HoYProg
setter ProgramMessage { _newProgram=program_ } = set program program_
setter EmptyMessage = programId

programId :: HoYProg -> HoYProg
programId program = program

createMessages :: Message -> [OSC.Message]
createMessages ProgramMessage { _newProgram=program_ } =
  [OSC.Message "/hoy/program" [OSC.d_put (OSC.ascii program_)]]
createMessages EmptyMessage = []

-- Utils

firstValue :: OSC.Datem a => [OSC.Datum] -> a
firstValue datum = fromJust $ OSC.d_get $ head datum
