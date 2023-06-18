module ReachProbs where

import Card (Card (Card), Numero (A, N2), Suit (Club, Diamond), cardIndex, cardsCount)
import CardPair (CardPair (cards, weight), cardPairIndex, cardsFromPairIndex)
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as IM
import Data.Ix (Ix (index))
import qualified Data.Vector.Unboxed as VU
import Range (Range)

type Probability = Double

newtype ReachProbs = ReachProbs {probs :: IntMap Probability}

reachProbsFromRange :: Range -> ReachProbs
reachProbsFromRange range = ReachProbs {probs = _probs}
  where
    _probs = IM.fromList (map f range)
    f cardPair = (cardPairIndex cardPair, weight cardPair)

probFromCardPair :: ReachProbs -> CardPair -> Probability
probFromCardPair reachProbs cardPair = probs reachProbs ! cardPairIndex cardPair

probsPerCards :: ReachProbs -> VU.Vector Double
probsPerCards reachProbs = VU.accum (+) emptyVector cardIdxAndProbs
  where
    emptyVector = VU.replicate cardsCount 0.0
    cardPairIdxAndProbs = IM.assocs (probs reachProbs)
    cardIdxAndProbs = concatMap (\(k, v) -> let (card1, card2) = cardsFromPairIndex k in [(cardIndex card1, v), (cardIndex card2, v)]) cardPairIdxAndProbs
