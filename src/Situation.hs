module Situation where

import Card (Card)
import Player (Player (IP, OOP), nextPlayer)
import Round (Round, nextRound)
import Text.Printf (printf)

data Situation = Situation
  { round :: Round,
    ipcommit :: Double,
    oopcommit :: Double,
    restcommit :: Double,
    stack :: Double,
    deck :: [Card],
    player :: Player,
    raiseTimesRemaining :: Int
  }
  deriving (Eq)

instance Show Situation where
  show s = printf "Situation {%s}" ins
    where
      ins = printf "%s ip/oop/rest = %f/%f/%f stack = %f deck = %s for %s rtr = %d" (show $ Situation.round s) (ipcommit s) (oopcommit s) (restcommit s) (stack s) (show $ deck s) (show $ player s) (raiseTimesRemaining s) :: String

withNextPlayer :: Situation -> Situation
withNextPlayer s =
  Situation
    { Situation.round = Situation.round s,
      ipcommit = ipcommit s,
      oopcommit = oopcommit s,
      restcommit = restcommit s,
      stack = stack s,
      deck = deck s,
      player = nextPlayer $ player s,
      raiseTimesRemaining = raiseTimesRemaining s
    }

withNextRound :: Situation -> Situation
withNextRound s =
  Situation
    { Situation.round = nextRound $ Situation.round s,
      ipcommit = ipcommit s,
      oopcommit = oopcommit s,
      restcommit = restcommit s,
      stack = stack s,
      deck = deck s,
      player = OOP,
      raiseTimesRemaining = raiseTimesRemaining s
    }

withIpCommitIncrement :: Situation -> Double -> Situation
withIpCommitIncrement s inc =
  Situation
    { Situation.round = Situation.round s,
      ipcommit = ipcommit s + inc,
      oopcommit = oopcommit s,
      restcommit = restcommit s,
      stack = stack s,
      deck = deck s,
      player = player s,
      raiseTimesRemaining = raiseTimesRemaining s
    }

withOopCommitIncrement :: Situation -> Double -> Situation
withOopCommitIncrement s inc =
  Situation
    { Situation.round = Situation.round s,
      ipcommit = ipcommit s,
      oopcommit = oopcommit s + inc,
      restcommit = restcommit s,
      stack = stack s,
      deck = deck s,
      player = player s,
      raiseTimesRemaining = raiseTimesRemaining s
    }

withRaiseLimitDecrement :: Situation -> Situation
withRaiseLimitDecrement s =
  Situation
    { Situation.round = Situation.round s,
      ipcommit = ipcommit s,
      oopcommit = oopcommit s,
      restcommit = restcommit s,
      stack = stack s,
      deck = deck s,
      player = player s,
      raiseTimesRemaining = raiseTimesRemaining s - 1
    }

pot :: Situation -> Double
pot s = ipcommit s + oopcommit s

playersCommit :: Situation -> Player -> Double
playersCommit s IP = ipcommit s
playersCommit s OOP = oopcommit s

initialEffectiveStack :: Situation -> Double
initialEffectiveStack s = stack s - oopcommit s
