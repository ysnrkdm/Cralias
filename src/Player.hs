module Player where

data Player = IP | OOP deriving (Show, Eq, Ord, Enum)

nextPlayer :: Player -> Player
nextPlayer IP = OOP
nextPlayer OOP = IP
