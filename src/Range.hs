{-# LANGUAGE OverloadedStrings #-}

module Range where

import Card (Card (Card), Numero, Suit (Club, Diamond, Heart, Spade), cardBitmapFromCards, numeroFromChar, suits)
import CardPair (CardPair, cardBitmapFromPair, cardPairFromCards, cardPairFromCardsWithWeight)
import Data.Bits (Bits (popCount), (.&.))
import Data.List.Split (splitOn)
import FlowUtil ((|>))
import Text.Regex.TDFA (AllTextMatches (getAllTextMatches), (=~))

type Range = [CardPair]

cardsOffSuited :: Numero -> Numero -> [(Card, Card)]
cardsOffSuited numero1 numero2 = map (\(s1, s2) -> (Card s1 numero1, Card s2 numero2)) suitsOffSuited
  where
    suitsOffSuited = [(x, y) | x <- suits, y <- suits, x /= y]

cardsSuited :: Numero -> Numero -> [(Card, Card)]
cardsSuited numero1 numero2 = map (\suit -> (Card suit numero1, Card suit numero2)) suits

rangeCardFromString :: String -> [(Card, Card)]
rangeCardFromString str
  | length str == 2 && numero1 == numero2 = cardsOffSuited numero1 numero2
  | length str == 2 = cardsSuited numero1 numero2 ++ cardsOffSuited numero1 numero2
  | length str == 3 && last str == 's' = cardsSuited numero1 numero2
  | length str == 3 && last str == 'o' = cardsOffSuited numero1 numero2
  | otherwise = undefined
  where
    numero1 = numeroFromChar (head str)
    numero2 = numeroFromChar (str !! 1)

cardPairsFromString :: Double -> String -> [CardPair]
cardPairsFromString weight str = map (uncurry (cardPairFromCardsWithWeight weight)) $ rangeCardFromString str

rangeForSingleWeight :: String -> [CardPair]
rangeForSingleWeight str = concatMap (cardPairsFromString weight) cardsString
  where
    parsedString = splitOn ":" str
    cardStrings = head parsedString
    weightString = last parsedString
    cardsString = splitOn "," cardStrings
    weight = read weightString :: Double

rangeFromString :: String -> Range
rangeFromString str = concatMap rangeForSingleWeight rangeStrings
  where
    rangeStrings = rangeStrFromString str

rangeStrFromString :: String -> [String]
rangeStrFromString str = map init $ getAllTextMatches (strRectified =~ ("[AKQJT2-9os,]+:[0-9\\.]+," :: String))
  where
    strRectified = if last str == ',' then str else str ++ ","

filterBoardCards :: Range -> [Card] -> Range
filterBoardCards range boardCards =
  range
    |> map (\v -> (cardBitmapFromPair v, v))
    |> filter (\v -> popCount (filterBitmap .&. fst v) == 0)
    |> map snd
  where
    filterBitmap = cardBitmapFromCards boardCards
