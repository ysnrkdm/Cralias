module UnitTest where

import Card (fromString)
import Debug.Trace (trace)
import GameSetting
import GameTree
import Player
import Round
import Situation
import Test.Tasty
import Test.Tasty.HUnit

streetSettingFT =
  StreetSetting
    { betSizes = [0.5, 0.66],
      raiseSizes = [0.66],
      donkSizes = [],
      allIn = False
    }

streetSettingR =
  StreetSetting
    { betSizes = [0.5, 0.66],
      raiseSizes = [0.66, 1.0],
      donkSizes = [],
      allIn = False
    }

gameSetting =
  GameSetting
    { flopIp = streetSettingFT,
      turnIp = streetSettingFT,
      riverIp = streetSettingR,
      flopOop = streetSettingFT,
      turnOop = streetSettingFT,
      riverOop = streetSettingR,
      maxRaiseTimes = 3,
      smallBlind = 0.5,
      bigBlind = 1,
      allInThreshold = 0.67
    }

preflopSituation =
  Situation
    { Situation.round = Flop,
      ipcommit = 3,
      oopcommit = 3,
      restcommit = 3.5,
      stack = 100,
      deck = map Card.fromString ["Ad", "8h", "5s"],
      player = OOP,
      raiseTimesRemaining = 3
    }

unit_base1 = trace ("\n" ++ show actual) $ expected @=? actual
  where
    actual = (gametree gameSetting preflopSituation)
    expected = (Node {node = (preflopSituation, Action GameTree.RoundBegin), childNodes = []})
