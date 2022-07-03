{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Book () where

import Data.Char (isAlpha, isSpace)
import Data.Maybe (mapMaybe)
import Numeric.Natural (Natural)

-- Letters: A String that only accept letters
newtype Letters = Letters String deriving newtype (Show, Read)

letter :: Char -> Maybe Char
letter c
  | isAlpha c = Just c
  | isSpace c = Just c
  | otherwise = Nothing

-- TODO: Decide if continue deleting non letters or send a message.
toLetters :: String -> Letters
toLetters = Letters . mapMaybe letter

newtype FullName = FullName Letters deriving newtype (Show, Read)

toFullName :: String -> FullName
toFullName = FullName . toLetters

data Person
  = PersonFullName FullName
  | Alias String
  | Anonymous

instance Show Person where
  show (PersonFullName s) = show s
  show (Alias s) = s
  show Anonymous = "Anonymous"

newtype Title = Title String deriving newtype (Show, Read)

newtype Language = Language [Letters] deriving newtype (Show, Read)

newtype NoPages = NoPages Natural

newtype NoWords = NoWords Natural

newtype Editorial = Editorial String deriving newtype (Show, Read)

newtype Author = Author Person deriving newtype (Show)

newtype Translator = Translator Person deriving newtype (Show)

data BookFormat
  = Physical
  | Digital
  deriving (Show, Read)

{--
instance Show BookFormat where
  show Physical = "Physical"
  show Digital = "Digital"
--}

newtype Editor = Editor Person deriving newtype (Show)

newtype Ilustrator = Ilustrator Person deriving newtype (Show)

-- TODO: create a tyoe for published date and publishing period
-- TODO: decide if use a enum type or a type class for categories and genres
-- TODO: decide how to aproach optionality before creating book type

ulisesArguelles :: FullName
-- ulisesArguelles = FullName (LastName $ letterString "Arguelles", FirstName $ letterString "Ulises")
ulisesArguelles = toFullName "Arguelles, Ulises"

-- fullNameUAAM = FullNameM (LastName $ letterString "Arguelles Monjaraz", FirstName $ letterString "Ulises Alexander")
fullNameUAAM :: FullName
fullNameUAAM = toFullName "Arguelles Monjaraz, Ulises Alexander"

testLetter :: IO ()
testLetter = print $ toLetters "c"

testLetters2 :: IO ()
testLetters2 = print $ toLetters "sa54ss"

testWord :: IO ()
testWord = print $ toLetters "test"

testPerson :: IO ()
testPerson = print ulisesArguelles

testPerson2 :: IO ()
testPerson2 = print fullNameUAAM
