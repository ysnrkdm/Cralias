module GameSetting where

import Player (Player (IP, OOP))
import Range (Range)
import Round (Round (Flop, Preflop, River, Turn))
import Situation (Situation (player, round))

data StreetSetting = StreetSetting
  { betSizes :: [Double],
    raiseSizes :: [Double],
    donkSizes :: [Double],
    allIn :: Bool
  }

data BetType = DonkType | BetType | RaiseType deriving (Show, Eq)

sizesFromBetType :: StreetSetting -> BetType -> [Double]
sizesFromBetType ss DonkType = donkSizes ss
sizesFromBetType ss BetType = betSizes ss
sizesFromBetType ss RaiseType = raiseSizes ss

data GameSetting = GameSetting
  { flopIp :: StreetSetting,
    turnIp :: StreetSetting,
    riverIp :: StreetSetting,
    flopOop :: StreetSetting,
    turnOop :: StreetSetting,
    riverOop :: StreetSetting,
    maxRaiseTimes :: Int,
    smallBlind :: Double,
    bigBlind :: Double,
    allInThreshold :: Double,
    ipRange :: Range,
    oopRange :: Range
  }

_streetSetting :: GameSetting -> Round -> Player -> StreetSetting
_streetSetting g Preflop _ = undefined
_streetSetting g Flop IP = flopIp g
_streetSetting g Flop OOP = flopOop g
_streetSetting g Turn IP = turnIp g
_streetSetting g Turn OOP = turnOop g
_streetSetting g River IP = riverIp g
_streetSetting g River OOP = riverOop g

streetSetting :: GameSetting -> Situation -> StreetSetting
streetSetting g s = _streetSetting g (Situation.round s) (player s)

playerRange :: GameSetting -> Player -> Range
playerRange g IP = ipRange g
playerRange g OOP = oopRange g
