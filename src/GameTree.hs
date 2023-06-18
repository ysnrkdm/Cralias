module GameTree where

import Card (Card)
import Data.List (nub, sort)
import FlowUtil ((|>))
import GameSetting (BetType (BetType, RaiseType), GameSetting (allInThreshold, bigBlind, smallBlind), StreetSetting (allIn), sizesFromBetType, streetSetting)
import Player (Player (IP, OOP), nextPlayer)
import Round (Round (River))
import Situation (Situation (deck, ipcommit, oopcommit, player, raiseTimesRemaining, round, stack), initialEffectiveStack, playersCommit, withIpCommitIncrement, withNextPlayer, withNextRound, withOopCommitIncrement, withRaiseLimitDecrement)
import Tree (Tree, reptree)

data NodeAction = RoundBegin | Begin | Bet {size :: Double} | Raise {size :: Double} | Check | Call | Fold deriving (Show, Eq)

data TableAction = TBet | TRaise | TCheck | TCall | TFold deriving (Show, Eq)

data Result
  = Action {action :: NodeAction}
  | Chance
      { cards :: [Card],
        donk :: Bool
      }
  | Showdown
      { tiePayoffs :: (Double, Double),
        playerPayoffs :: ((Double, Double), (Double, Double))
      }
  | Terminal
      { payoffs :: (Double, Double),
        winner :: Player
      }
  deriving (Show, Eq)

type NodeType = (Situation, Result)

roundToNearestBy :: Double -> Double -> Double
roundToNearestBy num by = fromIntegral (Prelude.round (num / by)) * by

baseBetSizeFromRatio :: GameSetting -> Situation -> Double -> Double
baseBetSizeFromRatio g s betPotRatio
  | oopCommit == sb = roundToNearestBy (betPotRatio * bb - sb) sb
  | ipCommit == bb && oopCommit == bb = roundToNearestBy (betPotRatio * bb) sb
  | otherwise = roundToNearestBy (betPotRatio * pot) bb
  where
    bb = bigBlind g
    sb = smallBlind g
    oopCommit = oopcommit s
    ipCommit = ipcommit s
    pot = max oopCommit ipCommit * 2

betSizeFromRatio :: GameSetting -> Situation -> GameSetting.BetType -> Double -> Double
betSizeFromRatio g s betType betPotRatio
  | betSizeConcl > 0 = min (stack s - curPlayerCommit) betSizeConcl
  | otherwise = 0
  where
    bb = bigBlind g
    sb = smallBlind g
    curPlayerCommit = playersCommit s $ player s
    nextPlayerCommit = playersCommit s $ nextPlayer $ player s
    initialEffStack = initialEffectiveStack s
    allInThresRatio = allInThreshold g
    allInThresBetSize = initialEffStack * allInThresRatio
    baseBetSize = baseBetSizeFromRatio g s betPotRatio
    betSizeRectifiedRaise = baseBetSize + (if betType == GameSetting.RaiseType then nextPlayerCommit - curPlayerCommit else 0)
    betSizeRectifiedAllIn = if betSizeRectifiedRaise + curPlayerCommit > allInThresBetSize then stack s - curPlayerCommit else betSizeRectifiedRaise
    betSizeConcl = betSizeRectifiedAllIn

possibleBetSizes :: GameSetting -> Situation -> BetType -> [Double]
possibleBetSizes g s betType =
  (betSizes ++ allInSizes)
    |> filter (> 0)
    |> filter possibleBetSizesFilter
    |> filter (> (nextPlayerCommit - curPlayerCommit))
    |> filter (<= (stack s - curPlayerCommit))
    |> sort
    |> nub
  where
    bb = bigBlind g
    sb = smallBlind g
    _streetSetting = streetSetting g s
    curPlayerCommit = playersCommit s $ player s
    nextPlayerCommit = playersCommit s $ nextPlayer $ player s
    betPotRatios = sizesFromBetType _streetSetting betType
    betSizes = map (betSizeFromRatio g s betType) betPotRatios
    allInSizes = [stack s - curPlayerCommit | allIn _streetSetting]
    possibleBetSizesFilter
      | curPlayerCommit == sb = (>= bb)
      | curPlayerCommit == bb && nextPlayerCommit == bb = (>= bb)
      | otherwise = (>= ((curPlayerCommit - nextPlayerCommit) * 2))

payoffsAndPeaceGetbackVec :: Situation -> (((Double, Double), (Double, Double)), (Double, Double))
payoffsAndPeaceGetbackVec s = (payoffs, peaceGetbackVec)
  where
    p1Commit = Situation.ipcommit s
    p2Commit = Situation.oopcommit s
    peace_getback = (p1Commit + p2Commit) / 2
    payoffs = ((p2Commit, -p2Commit), (-p1Commit, p1Commit))
    peaceGetbackVec = (peace_getback - p1Commit, peace_getback - p2Commit)

expandPossibleActions :: GameSetting -> Situation -> NodeAction -> TableAction -> [NodeType]
expandPossibleActions g s Call TCheck = case Situation.round s of
  River -> [(s, Showdown peaceGetbackVec payoffs)]
    where
      (payoffs, peaceGetbackVec) = payoffsAndPeaceGetbackVec s
  _ -> [(withNextRound s, Chance (Situation.deck s) False)]
expandPossibleActions g s prevAction TCheck = []
expandPossibleActions g s prevAction TBet = map (\v -> (nextSituation v, Action $ Bet v)) betSizes
  where
    betSizes = possibleBetSizes g s BetType
    nextSituation v = withNextPlayer $ case player s of
      IP -> withIpCommitIncrement s v
      OOP -> withOopCommitIncrement s v
expandPossibleActions g s prevAction TCall = case Situation.round s of
  River -> [(s, Showdown peaceGetbackVec payoffs)]
    where
      (payoffs, peaceGetbackVec) = payoffsAndPeaceGetbackVec s
  _ -> [(withNextRound s, Chance (Situation.deck s) (player s == OOP))]
expandPossibleActions g s prevAction TRaise = map (\v -> (nextSituation v, Action $ Raise v)) betSizes
  where
    betSizes = if raiseTimesRemaining s == 0 then [] else possibleBetSizes g s RaiseType
    nextSituation v = withNextPlayer $
      withRaiseLimitDecrement $ case player s of
        IP -> withIpCommitIncrement s v
        OOP -> withOopCommitIncrement s v
expandPossibleActions g s prevAction TFold = [(s, Terminal payoffs (nextPlayer (player s)))]
  where
    payoffs = case player s of
      IP -> (-(ipcommit s), ipcommit s)
      OOP -> (oopcommit s, -(oopcommit s))

expand :: GameSetting -> NodeType -> [NodeType]
expand g (situation, Action a) = concatMap (expandPossibleActions g situation a) possibleActions
  where
    possibleActions = case a of
      RoundBegin -> [TCheck, TBet]
      Begin -> [TCheck, TBet]
      Bet _ -> [TCall, TRaise, TFold]
      Raise _ -> [TCall, TRaise, TFold]
      Check -> [TCheck, TRaise, TBet]
      Call -> [TCheck, TRaise]
      Fold -> []
expand g (situation, Chance cards donk) = case Situation.round situation of
  River -> [(situation, Showdown peaceGetbackVec payoffs)]
    where
      (payoffs, peaceGetbackVec) = payoffsAndPeaceGetbackVec situation
  _ -> []
expand g (situation, result) = []

gametree :: GameSetting -> Situation -> Tree NodeType
gametree g s = reptree (expand g) (s, Action RoundBegin)
