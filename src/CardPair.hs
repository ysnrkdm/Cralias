module CardPair where

import Card (Card (), CardBitmap, cardBitmapFromCards, cardIndex)
import Data.Ix (Ix (index, range))
import Text.Printf (printf)

data CardPair = CardPair {cards :: (Card, Card), weight :: Double, relativeProb :: Double} deriving (Eq, Ord)

instance Show CardPair where
  show (CardPair cards weight relativeProb) = printf "CardPair {%s}" contents
    where
      contents = printf "%s, %f/%f" (show cards) weight relativeProb :: String

cardsListFromCardPair :: CardPair -> [Card]
cardsListFromCardPair cardPair = [card1, card2]
  where
    (card1, card2) = cards cardPair

cardPairIndex :: CardPair -> Int
cardPairIndex cardPair = index (minBound, maxBound) (cards cardPair)

cardsFromPairIndex :: Int -> (Card, Card)
cardsFromPairIndex cardPairIdx = cards
  where
    cards = (range (minBound, maxBound) :: [(Card, Card)]) !! cardPairIdx

cardPairFromCards :: Card -> Card -> CardPair
cardPairFromCards card1 card2
  | card1 < card2 = CardPair (card1, card2) 0 0
  | otherwise = CardPair (card2, card1) 0 0

cardPairFromCardsWithWeight :: Double -> Card -> Card -> CardPair
cardPairFromCardsWithWeight weight card1 card2
  | card1 < card2 = CardPair (card1, card2) weight 0
  | otherwise = CardPair (card2, card1) weight 0

withWeight :: CardPair -> Double -> CardPair
withWeight cardPair newWeight =
  CardPair
    { cards = cards cardPair,
      weight = newWeight,
      relativeProb = relativeProb cardPair
    }

cardBitmapFromPair :: CardPair -> CardBitmap
cardBitmapFromPair (CardPair (card1, card2) _ _) = cardBitmapFromCards [card1, card2]
