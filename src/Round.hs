module Round where

data Round = Preflop | Flop | Turn | River deriving (Show, Eq, Ord, Enum)

nextRound :: Round -> Round
nextRound Preflop = Flop
nextRound Flop = Turn
nextRound Turn = River
nextRound River = undefined
