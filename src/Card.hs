module Card where

import Data.Bits (Bits (shiftL, zeroBits, (.&.), (.|.)), FiniteBits (countTrailingZeros))
import Data.Ix (Ix (index))
import Data.List (foldl', unfoldr)
import Data.Word (Word64)

data Suit = Diamond | Spade | Heart | Club deriving (Eq, Ord, Enum, Bounded, Ix)

instance Show Suit where
  show Diamond = "d"
  show Spade = "s"
  show Heart = "h"
  show Club = "c"

suiteFromChar :: Char -> Suit
suiteFromChar 'd' = Diamond
suiteFromChar 's' = Spade
suiteFromChar 'h' = Heart
suiteFromChar 'c' = Club
suiteFromChar _ = undefined

suits :: [Suit]
suits = [Diamond, Spade, Heart, Club]

data Numero = A | K | Q | J | T | N9 | N8 | N7 | N6 | N5 | N4 | N3 | N2 deriving (Eq, Ord, Enum, Bounded, Ix)

instance Show Numero where
  show A = "A"
  show K = "K"
  show Q = "Q"
  show J = "J"
  show T = "T"
  show N9 = "9"
  show N8 = "8"
  show N7 = "7"
  show N6 = "6"
  show N5 = "5"
  show N4 = "4"
  show N3 = "3"
  show N2 = "2"

numeroFromChar :: Char -> Numero
numeroFromChar 'A' = A
numeroFromChar 'K' = K
numeroFromChar 'Q' = Q
numeroFromChar 'J' = J
numeroFromChar 'T' = T
numeroFromChar '9' = N9
numeroFromChar '8' = N8
numeroFromChar '7' = N7
numeroFromChar '6' = N6
numeroFromChar '5' = N5
numeroFromChar '4' = N4
numeroFromChar '3' = N3
numeroFromChar '2' = N2
numeroFromChar _ = undefined

data Card = Card {suite :: Suit, num :: Numero} deriving (Eq, Ix, Ord, Bounded)

cardFromString :: String -> Card
cardFromString [n, s] = Card (suiteFromChar s) (numeroFromChar n)
cardFromString _ = undefined

instance Show Card where
  show (Card suite num) = show num ++ show suite

cardIndex :: Card -> Int
cardIndex = index (minBound, maxBound)

allCards :: [Card]
allCards = [Card suit num | suit <- [minBound .. maxBound], num <- [minBound .. maxBound]]

cardsCount :: Int
cardsCount = 13 * 4 -- length allCards

type CardBitmap = Word64

cardBitmapFromCard :: Card -> CardBitmap
cardBitmapFromCard card = shiftL 1 (cardIndex card)

cardBitmapFromCards :: [Card] -> CardBitmap
cardBitmapFromCards cards = foldl' (.|.) zeroBits $ map cardBitmapFromCard cards

cardsFromCardBitmap :: CardBitmap -> [Card]
cardsFromCardBitmap = unfoldr f
  where
    f 0 = Nothing
    f i = Just (allCards !! countTrailingZeros i, i .&. (i - 1))
