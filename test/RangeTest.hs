module RangeTest where

import Card (cardFromString)
import CardPair (CardPair, cardPairFromCardsWithWeight)
import Data.List ((\\))
import Range (filterBoardCards, rangeFromString)
import Test.Tasty ()
import Test.Tasty.HUnit (Assertion, (@=?), (@?))
import TestUtils (($=?))
import Text.Printf (printf)

cardPairOf :: String -> String -> Double -> CardPair.CardPair
cardPairOf card1 card2 weight = cardPairFromCardsWithWeight weight (cardFromString card1) (cardFromString card2)

unit_rangeFromStringTest1 :: Assertion
unit_rangeFromStringTest1 = expected $=? actual
  where
    expected =
      [cardPairOf "Ad" "As" 0.75, cardPairOf "Ad" "Ah" 0.75, cardPairOf "Ad" "Ac" 0.75, cardPairOf "Ad" "As" 0.75, cardPairOf "As" "Ah" 0.75, cardPairOf "As" "Ac" 0.75, cardPairOf "Ad" "Ah" 0.75, cardPairOf "As" "Ah" 0.75, cardPairOf "Ah" "Ac" 0.75, cardPairOf "Ad" "Ac" 0.75, cardPairOf "As" "Ac" 0.75, cardPairOf "Ah" "Ac" 0.75]
        ++ [cardPairOf "Kd" "Ks" 0.75, cardPairOf "Kd" "Kh" 0.75, cardPairOf "Kd" "Kc" 0.75, cardPairOf "Kd" "Ks" 0.75, cardPairOf "Ks" "Kh" 0.75, cardPairOf "Ks" "Kc" 0.75, cardPairOf "Kd" "Kh" 0.75, cardPairOf "Ks" "Kh" 0.75, cardPairOf "Kh" "Kc" 0.75, cardPairOf "Kd" "Kc" 0.75, cardPairOf "Ks" "Kc" 0.75, cardPairOf "Kh" "Kc" 0.75]
        ++ [cardPairOf "Qd" "Qs" 0.75, cardPairOf "Qd" "Qh" 0.75, cardPairOf "Qd" "Qc" 0.75, cardPairOf "Qd" "Qs" 0.75, cardPairOf "Qs" "Qh" 0.75, cardPairOf "Qs" "Qc" 0.75, cardPairOf "Qd" "Qh" 0.75, cardPairOf "Qs" "Qh" 0.75, cardPairOf "Qh" "Qc" 0.75, cardPairOf "Qd" "Qc" 0.75, cardPairOf "Qs" "Qc" 0.75, cardPairOf "Qh" "Qc" 0.75]
        ++ [cardPairOf "Jd" "Js" 0.75, cardPairOf "Jd" "Jh" 0.75, cardPairOf "Jd" "Jc" 0.75, cardPairOf "Jd" "Js" 0.75, cardPairOf "Js" "Jh" 0.75, cardPairOf "Js" "Jc" 0.75, cardPairOf "Jd" "Jh" 0.75, cardPairOf "Js" "Jh" 0.75, cardPairOf "Jh" "Jc" 0.75, cardPairOf "Jd" "Jc" 0.75, cardPairOf "Js" "Jc" 0.75, cardPairOf "Jh" "Jc" 0.75]
        ++ [cardPairOf "Td" "Ts" 0.75, cardPairOf "Td" "Th" 0.75, cardPairOf "Td" "Tc" 0.75, cardPairOf "Td" "Ts" 0.75, cardPairOf "Ts" "Th" 0.75, cardPairOf "Ts" "Tc" 0.75, cardPairOf "Td" "Th" 0.75, cardPairOf "Ts" "Th" 0.75, cardPairOf "Th" "Tc" 0.75, cardPairOf "Td" "Tc" 0.75, cardPairOf "Ts" "Tc" 0.75, cardPairOf "Th" "Tc" 0.75]
        ++ [cardPairOf "9d" "9s" 0.75, cardPairOf "9d" "9h" 0.75, cardPairOf "9d" "9c" 0.75, cardPairOf "9d" "9s" 0.75, cardPairOf "9s" "9h" 0.75, cardPairOf "9s" "9c" 0.75, cardPairOf "9d" "9h" 0.75, cardPairOf "9s" "9h" 0.75, cardPairOf "9h" "9c" 0.75, cardPairOf "9d" "9c" 0.75, cardPairOf "9s" "9c" 0.75, cardPairOf "9h" "9c" 0.75]
        ++ [cardPairOf "8d" "8s" 0.75, cardPairOf "8d" "8h" 0.75, cardPairOf "8d" "8c" 0.75, cardPairOf "8d" "8s" 0.75, cardPairOf "8s" "8h" 0.75, cardPairOf "8s" "8c" 0.75, cardPairOf "8d" "8h" 0.75, cardPairOf "8s" "8h" 0.75, cardPairOf "8h" "8c" 0.75, cardPairOf "8d" "8c" 0.75, cardPairOf "8s" "8c" 0.75, cardPairOf "8h" "8c" 0.75]
        ++ [cardPairOf "7d" "7s" 0.50, cardPairOf "7d" "7h" 0.50, cardPairOf "7d" "7c" 0.50, cardPairOf "7d" "7s" 0.50, cardPairOf "7s" "7h" 0.50, cardPairOf "7s" "7c" 0.50, cardPairOf "7d" "7h" 0.50, cardPairOf "7s" "7h" 0.50, cardPairOf "7h" "7c" 0.50, cardPairOf "7d" "7c" 0.50, cardPairOf "7s" "7c" 0.50, cardPairOf "7h" "7c" 0.50]
        ++ [cardPairOf "6d" "6s" 0.25, cardPairOf "6d" "6h" 0.25, cardPairOf "6d" "6c" 0.25, cardPairOf "6d" "6s" 0.25, cardPairOf "6s" "6h" 0.25, cardPairOf "6s" "6c" 0.25, cardPairOf "6d" "6h" 0.25, cardPairOf "6s" "6h" 0.25, cardPairOf "6h" "6c" 0.25, cardPairOf "6d" "6c" 0.25, cardPairOf "6s" "6c" 0.25, cardPairOf "6h" "6c" 0.25]
        ++ [cardPairOf "5d" "5s" 0.25, cardPairOf "5d" "5h" 0.25, cardPairOf "5d" "5c" 0.25, cardPairOf "5d" "5s" 0.25, cardPairOf "5s" "5h" 0.25, cardPairOf "5s" "5c" 0.25, cardPairOf "5d" "5h" 0.25, cardPairOf "5s" "5h" 0.25, cardPairOf "5h" "5c" 0.25, cardPairOf "5d" "5c" 0.25, cardPairOf "5s" "5c" 0.25, cardPairOf "5h" "5c" 0.25]
        ++ [cardPairOf "Ad" "Kd" 0.75, cardPairOf "As" "Ks" 0.75, cardPairOf "Ah" "Kh" 0.75, cardPairOf "Ac" "Kc" 0.75, cardPairOf "Ad" "Ks" 0.75, cardPairOf "Ad" "Kh" 0.75, cardPairOf "Ad" "Kc" 0.75, cardPairOf "Kd" "As" 0.75, cardPairOf "As" "Kh" 0.75, cardPairOf "As" "Kc" 0.75, cardPairOf "Ah" "Kd" 0.75, cardPairOf "Ah" "Ks" 0.75, cardPairOf "Ah" "Kc" 0.75, cardPairOf "Kd" "Ac" 0.75, cardPairOf "Ks" "Ac" 0.75, cardPairOf "Kh" "Ac" 0.75]
        ++ [cardPairOf "Ad" "Qd" 0.75, cardPairOf "As" "Qs" 0.75, cardPairOf "Ah" "Qh" 0.75, cardPairOf "Ac" "Qc" 0.75, cardPairOf "Ad" "Qs" 0.75, cardPairOf "Ad" "Qh" 0.75, cardPairOf "Ad" "Qc" 0.75, cardPairOf "Qd" "As" 0.75, cardPairOf "As" "Qh" 0.75, cardPairOf "As" "Qc" 0.75, cardPairOf "Qd" "Ah" 0.75, cardPairOf "Qs" "Ah" 0.75, cardPairOf "Ah" "Qc" 0.75, cardPairOf "Qd" "Ac" 0.75, cardPairOf "Qs" "Ac" 0.75, cardPairOf "Qh" "Ac" 0.75]
        ++ [cardPairOf "Ad" "Jd" 0.50, cardPairOf "As" "Js" 0.50, cardPairOf "Ah" "Jh" 0.50, cardPairOf "Ac" "Jc" 0.50, cardPairOf "Ad" "Js" 0.50, cardPairOf "Ad" "Jh" 0.50, cardPairOf "Ad" "Jc" 0.50, cardPairOf "Jd" "As" 0.50, cardPairOf "As" "Jh" 0.50, cardPairOf "As" "Jc" 0.50, cardPairOf "Jd" "Ah" 0.50, cardPairOf "Js" "Ah" 0.50, cardPairOf "Ah" "Jc" 0.50, cardPairOf "Jd" "Ac" 0.50, cardPairOf "Js" "Ac" 0.50, cardPairOf "Jh" "Ac" 0.50]
    actual = rangeFromString "AA,KK,QQ,JJ,TT,99:0.75,88:0.75,77:0.5,66:0.25,55:0.25,AK,AQs,AQo:0.75,AJs,AJo:0.5"

unit_filterBoardCards :: Assertion
unit_filterBoardCards = expected $=? actual
  where
    expected =
      [cardPairOf "As" "Ah" 0.75, cardPairOf "As" "Ac" 0.75, cardPairOf "As" "Ah" 0.75, cardPairOf "Ah" "Ac" 0.75, cardPairOf "As" "Ac" 0.75, cardPairOf "Ah" "Ac" 0.75]
        ++ [cardPairOf "Kd" "Ks" 0.75, cardPairOf "Kd" "Kh" 0.75, cardPairOf "Kd" "Kc" 0.75, cardPairOf "Kd" "Ks" 0.75, cardPairOf "Ks" "Kh" 0.75, cardPairOf "Ks" "Kc" 0.75, cardPairOf "Kd" "Kh" 0.75, cardPairOf "Ks" "Kh" 0.75, cardPairOf "Kh" "Kc" 0.75, cardPairOf "Kd" "Kc" 0.75, cardPairOf "Ks" "Kc" 0.75, cardPairOf "Kh" "Kc" 0.75]
        ++ [cardPairOf "Qd" "Qh" 0.75, cardPairOf "Qd" "Qc" 0.75, cardPairOf "Qd" "Qh" 0.75, cardPairOf "Qh" "Qc" 0.75, cardPairOf "Qd" "Qc" 0.75, cardPairOf "Qh" "Qc" 0.75]
        ++ [cardPairOf "Jd" "Js" 0.75, cardPairOf "Jd" "Jh" 0.75, cardPairOf "Jd" "Jc" 0.75, cardPairOf "Jd" "Js" 0.75, cardPairOf "Js" "Jh" 0.75, cardPairOf "Js" "Jc" 0.75, cardPairOf "Jd" "Jh" 0.75, cardPairOf "Js" "Jh" 0.75, cardPairOf "Jh" "Jc" 0.75, cardPairOf "Jd" "Jc" 0.75, cardPairOf "Js" "Jc" 0.75, cardPairOf "Jh" "Jc" 0.75]
        ++ [cardPairOf "Td" "Th" 0.75, cardPairOf "Td" "Tc" 0.75, cardPairOf "Td" "Th" 0.75, cardPairOf "Th" "Tc" 0.75, cardPairOf "Td" "Tc" 0.75, cardPairOf "Th" "Tc" 0.75]
        ++ [cardPairOf "9d" "9s" 0.75, cardPairOf "9d" "9h" 0.75, cardPairOf "9d" "9c" 0.75, cardPairOf "9d" "9s" 0.75, cardPairOf "9s" "9h" 0.75, cardPairOf "9s" "9c" 0.75, cardPairOf "9d" "9h" 0.75, cardPairOf "9s" "9h" 0.75, cardPairOf "9h" "9c" 0.75, cardPairOf "9d" "9c" 0.75, cardPairOf "9s" "9c" 0.75, cardPairOf "9h" "9c" 0.75]
        ++ [cardPairOf "8d" "8s" 0.75, cardPairOf "8d" "8h" 0.75, cardPairOf "8d" "8c" 0.75, cardPairOf "8d" "8s" 0.75, cardPairOf "8s" "8h" 0.75, cardPairOf "8s" "8c" 0.75, cardPairOf "8d" "8h" 0.75, cardPairOf "8s" "8h" 0.75, cardPairOf "8h" "8c" 0.75, cardPairOf "8d" "8c" 0.75, cardPairOf "8s" "8c" 0.75, cardPairOf "8h" "8c" 0.75]
        ++ [cardPairOf "7d" "7s" 0.50, cardPairOf "7d" "7h" 0.50, cardPairOf "7d" "7c" 0.50, cardPairOf "7d" "7s" 0.50, cardPairOf "7s" "7h" 0.50, cardPairOf "7s" "7c" 0.50, cardPairOf "7d" "7h" 0.50, cardPairOf "7s" "7h" 0.50, cardPairOf "7h" "7c" 0.50, cardPairOf "7d" "7c" 0.50, cardPairOf "7s" "7c" 0.50, cardPairOf "7h" "7c" 0.50]
        ++ [cardPairOf "6d" "6s" 0.25, cardPairOf "6d" "6h" 0.25, cardPairOf "6d" "6c" 0.25, cardPairOf "6d" "6s" 0.25, cardPairOf "6s" "6h" 0.25, cardPairOf "6s" "6c" 0.25, cardPairOf "6d" "6h" 0.25, cardPairOf "6s" "6h" 0.25, cardPairOf "6h" "6c" 0.25, cardPairOf "6d" "6c" 0.25, cardPairOf "6s" "6c" 0.25, cardPairOf "6h" "6c" 0.25]
        ++ [cardPairOf "5d" "5s" 0.25, cardPairOf "5d" "5h" 0.25, cardPairOf "5d" "5c" 0.25, cardPairOf "5d" "5s" 0.25, cardPairOf "5s" "5h" 0.25, cardPairOf "5s" "5c" 0.25, cardPairOf "5d" "5h" 0.25, cardPairOf "5s" "5h" 0.25, cardPairOf "5h" "5c" 0.25, cardPairOf "5d" "5c" 0.25, cardPairOf "5s" "5c" 0.25, cardPairOf "5h" "5c" 0.25]
        ++ [cardPairOf "As" "Ks" 0.75, cardPairOf "Ah" "Kh" 0.75, cardPairOf "Ac" "Kc" 0.75, cardPairOf "Kd" "As" 0.75, cardPairOf "As" "Kh" 0.75, cardPairOf "As" "Kc" 0.75, cardPairOf "Ah" "Kd" 0.75, cardPairOf "Ah" "Ks" 0.75, cardPairOf "Ah" "Kc" 0.75, cardPairOf "Kd" "Ac" 0.75, cardPairOf "Ks" "Ac" 0.75, cardPairOf "Kh" "Ac" 0.75]
        ++ [cardPairOf "Ah" "Qh" 0.75, cardPairOf "Ac" "Qc" 0.75, cardPairOf "Qd" "As" 0.75, cardPairOf "As" "Qh" 0.75, cardPairOf "As" "Qc" 0.75, cardPairOf "Qd" "Ah" 0.75, cardPairOf "Ah" "Qc" 0.75, cardPairOf "Qd" "Ac" 0.75, cardPairOf "Qh" "Ac" 0.75]
        ++ [cardPairOf "As" "Js" 0.50, cardPairOf "Ah" "Jh" 0.50, cardPairOf "Ac" "Jc" 0.50, cardPairOf "Jd" "As" 0.50, cardPairOf "As" "Jh" 0.50, cardPairOf "As" "Jc" 0.50, cardPairOf "Jd" "Ah" 0.50, cardPairOf "Js" "Ah" 0.50, cardPairOf "Ah" "Jc" 0.50, cardPairOf "Jd" "Ac" 0.50, cardPairOf "Js" "Ac" 0.50, cardPairOf "Jh" "Ac" 0.50]
    actual = filterBoardCards range $ map cardFromString ["Ad", "Qs", "Ts"]
    range = rangeFromString "AA,KK,QQ,JJ,TT,99:0.75,88:0.75,77:0.5,66:0.25,55:0.25,AK,AQs,AQo:0.75,AJs,AJo:0.5"
