module UnitTest where

import Card (cardFromString)
import GameSetting (GameSetting (..), StreetSetting (..))
import GameTree
  ( NodeAction (RoundBegin),
    Result (Action),
    gametree,
  )
import Player (Player (OOP))
import Round (Round (Flop))
import Situation (Situation (..))
import Test.Tasty ()
import Test.Tasty.HUnit (Assertion, (@=?))
import Tree (Tree (Node, childNodes, node))

streetSettingFT :: StreetSetting
streetSettingFT =
  StreetSetting
    { betSizes = [0.5, 0.66],
      raiseSizes = [0.66],
      donkSizes = [],
      allIn = False
    }

streetSettingR :: StreetSetting
streetSettingR =
  StreetSetting
    { betSizes = [0.5, 0.66],
      raiseSizes = [0.66, 1.0],
      donkSizes = [],
      allIn = False
    }

gameSetting :: GameSetting
gameSetting =
  GameSetting
    { flopIp = streetSettingFT,
      turnIp = streetSettingFT,
      riverIp = streetSettingR,
      flopOop = streetSettingFT,
      turnOop = streetSettingFT,
      riverOop = streetSettingR,
      maxRaiseTimes = 2,
      smallBlind = 0.5,
      bigBlind = 1,
      allInThreshold = 0.67,
      ipRange = [],
      oopRange = []
    }

preflopSituation :: Situation
preflopSituation =
  Situation
    { Situation.round = Flop,
      ipcommit = 3,
      oopcommit = 3,
      restcommit = 3.5,
      stack = 100,
      deck = map cardFromString ["Ad", "8h", "5s"],
      player = OOP,
      raiseTimesRemaining = 2
    }

unit_base1 :: Assertion
unit_base1 = expected @=? actual
  where
    actual = gametree gameSetting preflopSituation
    expected = (Node {node = (preflopSituation, Action GameTree.RoundBegin), childNodes = []})
