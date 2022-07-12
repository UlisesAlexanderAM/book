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

newtype AlternativeTitle = AlternativeTitle Title

mkAlternativeTitle :: String -> AlternativeTitle
mkAlternativeTitle = AlternativeTitle . Title

newtype Publisher = Publisher String

newtype Language = Language String

newtype NumPages = NumPages Nat.Natural

newtype NumWords = NumWords Nat.Natural

newtype PubDate = PubDate Calendar.Day

newtype PubPeriod = PubPeriod Calendar.CalendarDiffDays

newtype PeriodSinceLastPub = PeriodSinceLastPub Calendar.CalendarDiffDays

data ReadingStatus
  = Reading
  | Read
  | WantedToRead
  | Pause
  | Dropped

data PublishingStatus
  = Publishing
  | Finished
  | Hiatus
  | Cancelled
