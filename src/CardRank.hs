module CardRank where

import Card (Card, CardBitmap, cardBitmapFromCards, cardFromString)
import Data.Bits ((.|.))
import Data.List.Split (splitOn)
import qualified Data.Map as M
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE cardRankData #-}
cardRankData :: [String]
cardRankData = lines $ unsafePerformIO $ readFile "./card5_dic_sorted.txt"

type Rank = Int

parseLine :: String -> (CardBitmap, Rank)
parseLine lineString = (cards, rank)
  where
    cardsAndRanks = splitOn "," lineString
    cards = cardBitmapFromCards $ map cardFromString $ splitOn "-" $ head cardsAndRanks
    rank = read $ last cardsAndRanks :: Rank

cardRanks :: [(CardBitmap, Rank)]
cardRanks = map parseLine cardRankData

cardRankMap :: M.Map CardBitmap Rank
cardRankMap = M.fromList cardRanks

rankFromCards :: [Card] -> Rank
rankFromCards cards = M.findWithDefault 0 (cardBitmapFromCards cards) cardRankMap

rankFromCardsAndCardBitmap :: [Card] -> CardBitmap -> Rank
rankFromCardsAndCardBitmap cards cmap = M.findWithDefault 0 (cmap .|. cardBitmapFromCards cards) cardRankMap
