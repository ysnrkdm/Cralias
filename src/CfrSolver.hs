module CfrSolver where

import Card (CardBitmap, cardIndex, cardsCount)
import CardPair (CardPair (cards), cardPairIndex)
import CfrNode (CfrNode (CfrNode))
import Combos (combosSortedByRankDesc, riverCombos)
import Data.IntMap (findWithDefault)
import Data.List (foldl')
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as VU
import GameSetting (GameSetting (ipRange, oopRange), playerRange)
import GameTree (NodeType, Result (Action, Chance, Showdown, Terminal))
import Player (Player (IP, OOP), nextPlayer)
import ReachProbs (ReachProbs (probs), probsPerCards)
import Tree (Tree (Node, childNodes, node))

cfr :: GameSetting -> Player -> Tree NodeType -> ReachProbs -> Int -> CardBitmap -> Int -> Tree (CfrNode, NodeType)
-- Action
cfr g player Node {node = curNode@(situation, Action a), childNodes = c} reachProbs nthIter curBoard deal =
  Node {node = (CfrNode [] [] [], curNode), childNodes = []}
-- Chance
cfr g player Node {node = curNode@(situation, Chance _ _), childNodes = c} reachProbs nthIter curBoard deal =
  Node {node = (CfrNode [] [] [], curNode), childNodes = []}
-- Showdown
cfr g player Node {node = curNode@(situation, Showdown _ playerPayoffs), childNodes = []} reachProbs _ curBoard _ =
  Node {node = (CfrNode [] [] [], curNode), childNodes = []}
  where
    winPayoff = case player of
      OOP -> (fst . fst) playerPayoffs
      IP -> (snd . snd) playerPayoffs
    losePayoff = case player of
      OOP -> (fst . snd) playerPayoffs
      IP -> (snd . fst) playerPayoffs
    reachProbsPerCards = probsPerCards reachProbs
    playerCombos = combosSortedByRankDesc $ riverCombos (playerRange g player) curBoard
    opponentCombos = combosSortedByRankDesc $ riverCombos (playerRange g $ nextPlayer player) curBoard
    highestPlayerCombo = head playerCombos
cfr g player Node {node = (_, Showdown _ _), childNodes = _} _ _ _ _ = undefined
-- Terminal
cfr g player Node {node = curNode@(_, Terminal payoffs _), childNodes = []} reachProbs _ _ _ =
  Node {node = (CfrNode [] [] p, curNode), childNodes = []}
  where
    payoff = case player of
      OOP -> fst payoffs
      IP -> snd payoffs
    opponentHand = case player of
      OOP -> ipRange g
      IP -> oopRange g
    reachProbsPerCards = probsPerCards reachProbs
    reachProbSum = VU.sum reachProbsPerCards
    playerHands = playerRange g player
    p =
      [ payoff
          * ( reachProbSum
                - reachProbsPerCards ! cardIndex (fst $ cards pair)
                - reachProbsPerCards ! cardIndex (snd $ cards pair)
                - findWithDefault 0 (cardPairIndex pair) (probs reachProbs)
            )
        | pair <- playerHands
      ]
cfr g player Node {node = (_, Terminal _ _), childNodes = _} _ _ _ _ = undefined
