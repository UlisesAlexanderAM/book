module Book
  ( Book (..),
    BookFormat (..),
    BookSeries (..),
    Genre (..),
    Category (..),
    Tag (..),
    Isbn (..),
    Cover (..),
    Person (Anonymous),
    mkPersonName,
    mkPersonAlias,
    Author (..),
    Translator (..),
    Editor (..),
    Ilustrator (..),
    Title (..),
    mkAlternativeTitle,
    Publisher (..),
    Language (..),
    NumPages (..),
    NumWords (..),
    PubDate (..),
    PubPeriod (..),
    PeriodSinceLastPub (..),
    ReadingStatus (..),
    PublishingStatus (..),
  )
where

import qualified Data.Time.Calendar as Calendar (CalendarDiffDays, Day)
import qualified Numeric.Natural as Nat (Natural)

class Book a where
  changeReadingStatus :: a -> a
  changePubPeriod :: a -> a
  updatePeriodSinceLastPub :: a -> a
  updatePublishingStatus :: a -> a

class Book a => BookFormat a where
  hasPhysical :: a -> Bool
  hasDigital :: a -> Bool

class Book a => BookSeries a where
  addVolume :: a -> a

class Genre a where
  getGenre :: a -> String

class Category a where
  getCategory :: a -> String

class Tag a where
  getTag :: a -> String

class Isbn a where
  getIsbn :: a -> String
  mkIsbn :: String -> a

class Cover a where
  getCover :: a -> b
  mkCover :: b -> a

newtype Name = Name String

data Person
  = PersonName Name
  | Alias String
  | Anonymous

mkPersonName :: String -> Person
mkPersonName = PersonName . Name

mkPersonAlias :: String -> Person
mkPersonAlias = Alias

newtype Author = Author Person

newtype Translator = Translator Person

newtype Editor = Editor Person

newtype Ilustrator = Ilustrator Person

newtype Title = Title String

newtype Editorial = Editorial String

newtype NoPages = NoPages Natural

newtype NoWords = NoWords Natural

data BookFormat
  = Physical
  | Digital
  deriving (Show, Read)

newtype PubDate = PubDate Calendar.Day

newtype PubPeriod = PubPeriod Calendar.CalendarDiffDays

newtype PeriodSinceLastPubl = PeriodSinceLastPub Calendar.CalendarDiffDays

class BookGenre a where
  toGenre :: String -> a
  fromGenre :: a -> String

class BookCategory a where
  toCategory :: String -> a
  fromCategory :: a -> String

-- TODO: decide how to aproach optionality before creating book type
{-
ulisesArguelles :: Name
-- ulisesArguelles = FullName (LastName $ letterString "Arguelles", FirstName $ letterString "Ulises")
ulisesArguelles = toName "Arguelles, Ulises"

-- fullNameUAAM = FullNameM (LastName $ letterString "Arguelles Monjaraz", FirstName $ letterString "Ulises Alexander")
fullNameUAAM :: Name
fullNameUAAM = toName "Arguelles Monjaraz, Ulises Alexander"

testLetter :: IO ()
testLetter = print $ toLetters "c"

testLetters2 :: IO ()
testLetters2 = print $ toLetters "sa54ss"

testWord :: IO ()
testWord = print $ toLetters "test"

personUAAM1 :: Person
personUAAM1 = mkPersonFullName "Ulises Alexander Arguelles Monjaraz"

personUAAM2 :: Person
personUAAM2 = mkPersonAlias "UAAM"

personAnon :: Person
personAnon = Anonymous

testPerson1 :: IO ()
testPerson1 = print personUAAM1

testPerson2 :: IO ()
testPerson2 = print personUAAM2

testPerson3 :: IO ()
testPerson3 = print personAnon
-}
