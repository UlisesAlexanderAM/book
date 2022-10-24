module Main (main) where

import Book
import RIO
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners (TestTree (TestGroup))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit Tests"
    [ personTests
    ]

personTests :: TestTree
personTests =
  TestGroup
    "Person type tests"
    [ testCase "Two different names are not equal" $
        mkName "Ulises" == mkName "Alexander" @?= False,
      testCase "Two equal names are equal" $
        mkName "Ulises Alexander" == mkName "Ulises Alexander" @?= True,
      testCase "Show Name" $
        (tshow . mkName $ "Ulises Alexander") == tshow ("Ulises Alexander" :: Text) @?= True
    ]
