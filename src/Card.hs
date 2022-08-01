module Card where

import Data.Ix (Ix)

data Suite = D | S | H | C deriving (Eq, Ord, Enum, Ix)

instance Show Suite where
  show D = "d"
  show S = "s"
  show H = "h"
  show C = "c"

suiteFromChar :: Char -> Suite
suiteFromChar 'd' = D
suiteFromChar 's' = S
suiteFromChar 'h' = H
suiteFromChar 'c' = C
suiteFromChar _ = undefined

data Numero = A | K | Q | J | T | N9 | N8 | N7 | N6 | N5 | N4 | N3 | N2 deriving (Eq, Ord, Enum, Ix)

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

data Card = Card {suite :: Suite, num :: Numero} deriving (Eq)

fromString :: String -> Card
fromString [n, s] = Card (suiteFromChar s) (numeroFromChar n)
fromString _ = undefined

instance Show Card where
  show (Card suite num) = show suite ++ show num
