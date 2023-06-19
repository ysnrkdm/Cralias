module CfrSolver where

import Card (CardBitmap, cardIndex, cardsCount)
import CardPair (CardPair (cards), cardPairIndex)
import CfrNode (CfrNode (CfrNode))
import Combos (RiverCombo (privateCards, rank), combosSortedByRankDesc, riverCombos)
import Data.IntMap (findWithDefault)
import qualified Data.IntMap as IM
import Data.List (foldl', scanl')
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as VU
import GameSetting (GameSetting (ipRange, oopRange), playerRange)
import GameTree (NodeType, Result (Action, Chance, Showdown, Terminal))
import Player (Player (IP, OOP), nextPlayer)
import ReachProbs (ReachProbs (probs), probsPerCards)
import Tree (Tree (Node, childNodes, node))

sumTuple3 :: (Num a) => [(a, a, a)] -> (a, a, a)
sumTuple3 = foldl' (\(acca, accb, accc) (a, b, c) -> (acca + a, accb + b, accc + c)) (0, 0, 0)

cfr :: GameSetting -> Player -> Tree NodeType -> ReachProbs -> Int -> CardBitmap -> Int -> Tree (CfrNode, NodeType)
-- Action
cfr g player Node {node = curNode@(situation, Action a), childNodes = c} reachProbs nthIter curBoard deal =
  Node {node = (CfrNode [] [] IM.empty, curNode), childNodes = []}
-- Chance
cfr g player Node {node = curNode@(situation, Chance _ _), childNodes = c} reachProbs nthIter curBoard deal =
  Node {node = (CfrNode [] [] IM.empty, curNode), childNodes = []}
-- Showdown
cfr g player Node {node = curNode@(situation, Showdown _ playerPayoffs), childNodes = []} reachProbs _ curBoard _ =
  Node {node = (CfrNode [] [] p, curNode), childNodes = []}
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
    playerComboDiffs = zip playerCombos (tail playerCombos)
    oppoCombosPartitionedL = [(fst pc, [j | j <- opponentCombos, rank j >= (rank . fst) pc && rank j < (rank . snd) pc]) | pc <- playerComboDiffs]
    oppoCombosPartitionedU = [(snd pc, [j | j <- opponentCombos, rank j > (rank . fst) pc && rank j <= (rank . snd) pc]) | pc <- playerComboDiffs]
    winsumPerOppoComboByCardPairIndex =
      IM.fromListWith (+) $
        scanl'
          (\(_, acc) (cardPairIndex, x) -> (cardPairIndex, acc + x))
          (0, 0)
          [ ( (cardPairIndex . privateCards) corrPlayerCombo,
              sum
                [ findWithDefault 0 (cardPairIndex (privateCards oppoCombo)) (probs reachProbs)
                  | oppoCombo <- oppoCombos
                ]
            )
            | (corrPlayerCombo, oppoCombos) <- oppoCombosPartitionedL
          ]
    winCardProbsByCardIndex =
      IM.fromListWith
        (+)
        $ concat
          [ concat
              [ [ (cardIndex ((fst . cards) (privateCards oppoCombo)), reachProbsPerCards ! cardIndex ((fst . cards) (privateCards oppoCombo))),
                  (cardIndex ((snd . cards) (privateCards oppoCombo)), reachProbsPerCards ! cardIndex ((snd . cards) (privateCards oppoCombo)))
                ]
                | oppoCombo <- oppoCombos
              ]
            | (corrPlayerCombo, oppoCombos) <- oppoCombosPartitionedL
          ]
    winpayoffs =
      IM.fromList
        [ ( cardPairIndex (privateCards pRivCombo),
            ( findWithDefault 0 (cardPairIndex (privateCards pRivComboUpperBound)) winsumPerOppoComboByCardPairIndex
                - findWithDefault 0 (cardIndex $ (fst . cards) (privateCards pRivCombo)) winCardProbsByCardIndex
                - findWithDefault 0 (cardIndex $ (snd . cards) (privateCards pRivCombo)) winCardProbsByCardIndex
            )
              * winPayoff
          )
          | (pRivCombo, pRivComboUpperBound) <- playerComboDiffs
        ]

    losssumPerOppoComboByCardPairIndex =
      IM.fromListWith (+) $
        scanl'
          (\(_, acc) (cardPairIndex, x) -> (cardPairIndex, acc + x))
          (0, 0)
          [ ( (cardPairIndex . privateCards) corrPlayerCombo,
              sum
                [ findWithDefault 0 (cardPairIndex (privateCards oppoCombo)) (probs reachProbs)
                  | oppoCombo <- oppoCombos
                ]
            )
            | (corrPlayerCombo, oppoCombos) <- oppoCombosPartitionedU
          ]
    lossCardProbsByCardIndex =
      IM.fromListWith
        (+)
        $ concat
          [ concat
              [ [ (cardIndex ((fst . cards) (privateCards oppoCombo)), reachProbsPerCards ! cardIndex ((fst . cards) (privateCards oppoCombo))),
                  (cardIndex ((snd . cards) (privateCards oppoCombo)), reachProbsPerCards ! cardIndex ((snd . cards) (privateCards oppoCombo)))
                ]
                | oppoCombo <- oppoCombos
              ]
            | (corrPlayerCombo, oppoCombos) <- oppoCombosPartitionedU
          ]
    losspayoffs =
      IM.fromList
        [ ( cardPairIndex (privateCards pRivCombo),
            ( findWithDefault 0 (cardPairIndex (privateCards pRivComboUpperBound)) losssumPerOppoComboByCardPairIndex
                - findWithDefault 0 (cardIndex $ (fst . cards) (privateCards pRivCombo)) lossCardProbsByCardIndex
                - findWithDefault 0 (cardIndex $ (snd . cards) (privateCards pRivCombo)) lossCardProbsByCardIndex
            )
              * winPayoff
          )
          | (pRivCombo, pRivComboUpperBound) <- playerComboDiffs
        ]
    p = IM.unionWith (+) winpayoffs losspayoffs
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
      IM.fromList
        [ ( cardPairIndex pair,
            payoff
              * ( reachProbSum
                    - reachProbsPerCards ! cardIndex (fst $ cards pair)
                    - reachProbsPerCards ! cardIndex (snd $ cards pair)
                    - findWithDefault 0 (cardPairIndex pair) (probs reachProbs)
                )
          )
          | pair <- playerHands
        ]
cfr g player Node {node = (_, Terminal _ _), childNodes = _} _ _ _ _ = undefined
