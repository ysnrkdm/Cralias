module CfrNode where

alpha :: Double
alpha = 1.5

beta :: Double
beta = 0.5

gamma :: Integer
gamma = 2

theta :: Double
theta = 0.9

type NumbersOnCards = [Double]

data CfrNode = CfrNode {regret :: NumbersOnCards, ev :: NumbersOnCards, payoffs :: NumbersOnCards} deriving (Eq, Show)

averageStrategy :: CfrNode -> NumbersOnCards
averageStrategy cfrNode = [0.0]

currentStrategy :: CfrNode -> NumbersOnCards
currentStrategy cfrNode = [0.0]
