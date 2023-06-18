module CardTest where

import Card
import Test.Tasty.HUnit (Assertion, (@=?))

unit_cardIndexTest :: Assertion
unit_cardIndexTest = expected @=? actual
  where
    expected = [0 .. 51]
    actual = map cardIndex allCards

unit_cardsBitmapTest1 :: Assertion
unit_cardsBitmapTest1 = expected @=? actual
  where
    expected = 35253091565569
    actual = cardBitmapFromCards $ map cardFromString ["Ad", "8c", "4h"]

unit_cardsFromCardBitmapTest1 :: Assertion
unit_cardsFromCardBitmapTest1 = expected @=? actual
  where
    expected = map cardFromString ["Ad", "Qs", "7s", "2s", "Jh"]
    actual = cardsFromCardBitmap (cardBitmapFromCards $ map cardFromString ["Ad", "Qs", "Jh", "7s", "2s"])
