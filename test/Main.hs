module Main (main) where

import Book (mkName)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Runners (TestTree (TestGroup))
import Universum

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
        (show . mkName $ ("Ulises Alexander" :: Text)) == (show ("Ulises Alexander" :: Text)) @?= True
    ]
