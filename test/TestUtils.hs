module TestUtils where

import Data.List ((\\))
import Test.Tasty.HUnit (Assertion, (@?))

($=?) :: (Show a, Eq a) => [a] -> [a] -> Assertion
expected $=? actual = null ea && null ae @? ("Expected only: " ++ show ea ++ ", Actual only: " ++ show ae)
  where
    ea = expected \\ actual
    ae = actual \\ expected
