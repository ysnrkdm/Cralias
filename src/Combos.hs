module Combos where

import Card (Card, CardBitmap, cardBitmapFromCards)
import CardPair (CardPair, cardsListFromCardPair)
import CardRank (Rank, rankFromCardsAndCardBitmap)
import Data.Bits ((.|.))
import Data.List (sortOn)
import Data.Ord (Down (Down))
import Range (Range)

data RiverCombo = RiverCombo {privateCards :: CardPair, allBoard :: CardBitmap, rank :: Rank}

riverCombos :: Range -> CardBitmap -> [RiverCombo]
riverCombos range currentBoard = map f range
  where
    f cardPair = RiverCombo {privateCards = cardPair, allBoard = privAndCommuCards, rank = rankFromCardsAndCardBitmap cardsInCardPair currentBoard}
      where
        cardsInCardPair = cardsListFromCardPair cardPair
        privAndCommuCards = currentBoard .|. cardBitmapFromCards cardsInCardPair

combosSortedByRankDesc :: [RiverCombo] -> [RiverCombo]
combosSortedByRankDesc = sortOn (Down . rank)
