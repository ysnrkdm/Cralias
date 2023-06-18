module CardRankTest where

import Card
import CardRank
import Test.Tasty.HUnit (Assertion, (@=?))

unit_cardRanksBase1 = expected @=? actual
  where
    actual = length cardRanks
    expected = 2598960

unit_cardRanksCont1 = expected @=? actual
  where
    actual = cardRanks !! 1
    expected = (cardBitmapFromCards $ map cardFromString ["2c", "2d", "2h", "2s", "3d"], 166)
