module CfrSolverTest where

import Card
import CfrNode
import CfrSolver
import GameSetting
import GameTree
import Player
import Range
import ReachProbs
import Round
import Situation
import Test.Tasty.HUnit (Assertion, (@=?))
import Tree

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
      ipRange = rangeFromString "AA,KK,QQ,JJ,TT,99:0.75,88:0.75,77:0.5,66:0.25,55:0.25,AK,AQs,AQo:0.75,AJs,AJo:0.5,ATs:0.75,A6s:0.25,A5s:0.75,A4s:0.75,A3s:0.5,A2s:0.5,KQs,KQo:0.5,KJs,KTs:0.75,K5s:0.25,K4s:0.25,QJs:0.75,QTs:0.75,Q9s:0.5,JTs:0.75,J9s:0.75,J8s:0.75,T9s:0.75,T8s:0.75,T7s:0.75,98s:0.75,97s:0.75,96s:0.5,87s:0.75,86s:0.5,85s:0.5,76s:0.75,75s:0.5,65s:0.75,64s:0.5,54s:0.75,53s:0.5,43s:0.5",
      oopRange = rangeFromString "QQ:0.5,JJ:0.75,TT,99,88,77,66,55,44,33,22,AKo:0.25,AQs,AQo:0.75,AJs,AJo:0.75,ATs,ATo:0.75,A9s,A8s,A7s,A6s,A5s,A4s,A3s,A2s,KQ,KJ,KTs,KTo:0.5,K9s,K8s,K7s,K6s,K5s,K4s:0.5,K3s:0.5,K2s:0.5,QJ,QTs,Q9s,Q8s,Q7s,JTs,JTo:0.5,J9s,J8s,T9s,T8s,T7s,98s,97s,96s,87s,86s,76s,75s,65s,64s,54s,53s,43s"
    }

flopSituation :: Situation
flopSituation =
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

unit_cfrTerminalBase = expected @=? actual
  where
    expected = Node {node = (CfrNode [] [] [], curNode), childNodes = []}
    actual = cfr gameSetting OOP Node {node = curNode, childNodes = []} reachProbs 0 0 0
    curNode = (flopSituation, Terminal (15, -20) OOP)
    reachProbs = reachProbsFromRange $ rangeFromString "AA,KK,QQ,JJ,TT,99:0.75,88:0.75,77:0.5,66:0.25,55:0.25,AK,AQs,AQo:0.75,AJs,AJo:0.5"
