module Main (main) where

import Book (mkPersonName)
import RIO
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit Tests"
    [ testCase "Two different person names are not equal" $
        mkPersonName "Ulises" == mkPersonName "Alexander" @?= False,
      testCase "Two equal person names are equal" $
        mkPersonName "Ulises Alexander" == mkPersonName "Ulises Alexander" @?= True
    ]
