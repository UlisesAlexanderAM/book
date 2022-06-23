module Book () where

import Data.Char (isAlpha)
import Data.Maybe (mapMaybe)
import Prelude hiding (Word)

newtype Letter = Letter Char deriving (Show)

letter :: Char -> Maybe Letter
letter c =
  if isAlpha c
    then Just $ Letter c
    else Nothing

letterString :: String -> [Letter]
letterString = mapMaybe letter

type Word = [Letter]

newtype FirstName = FirstName Word deriving (Show)

newtype LastName = LastName Word deriving (Show)

data FullName
  = OnlyName Word
  | FullName (LastName, FirstName)
  | FullNameM ([LastName], [FirstName])

data Person
  = Person FullName
  | Alias String
  | Anonymous
