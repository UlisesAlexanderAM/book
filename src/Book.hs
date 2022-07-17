{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Book
  ( -- * Book classes
    Book (..),
    BookFormat (..),
    BookSeries (..),
    Genre (..),
    Category (..),
    Tag (..),
    Isbn (..),
    Cover (..),

    -- * Book types
    Person (Anonymous),
    Author (..),
    Translator (..),
    Editor (..),
    Ilustrator (..),
    Title (..),
    Publisher (..),
    Language (..),
    NumPages (..),
    NumWords (..),
    PubDate (..),
    PubPeriod (..),
    PeriodSinceLastPub (..),
    ReadingStatus (..),
    PublishingStatus (..),

    -- * Custom constructors
    mkPersonAlias,
    mkAlternativeTitle,
    mkPersonName,
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

newtype Name = Name String deriving newtype (Eq, Show)

data Person
  = PersonName Name
  | Alias String
  | Anonymous
  deriving stock (Eq, Show)

mkPersonName :: String -> Person
mkPersonName = PersonName . Name

mkPersonAlias :: String -> Person
mkPersonAlias = Alias

newtype Author = Author Person deriving newtype (Eq, Show)

newtype Translator = Translator Person deriving newtype (Eq, Show)

newtype Editor = Editor Person deriving newtype (Eq, Show)

newtype Ilustrator = Ilustrator Person deriving newtype (Eq, Show)

newtype Title = Title String deriving newtype (Eq, Show)

newtype AlternativeTitle = AlternativeTitle Title deriving newtype (Eq, Show)

mkAlternativeTitle :: String -> AlternativeTitle
mkAlternativeTitle = AlternativeTitle . Title

newtype Publisher = Publisher String deriving newtype (Eq, Show)

newtype Language = Language String deriving newtype (Eq, Show)

newtype NumPages = NumPages Nat.Natural deriving newtype (Eq, Show)

newtype NumWords = NumWords Nat.Natural deriving newtype (Eq, Show)

newtype PubDate = PubDate Calendar.Day deriving newtype (Eq, Show)

newtype PubPeriod = PubPeriod Calendar.CalendarDiffDays deriving newtype (Eq, Show)

newtype PeriodSinceLastPub = PeriodSinceLastPub Calendar.CalendarDiffDays deriving newtype (Eq, Show)

data ReadingStatus
  = Reading
  | Read
  | WantedToRead
  | Pause
  | Dropped
  deriving stock (Eq, Show)

data PublishingStatus
  = Publishing
  | Finished
  | Hiatus
  | Cancelled
  deriving stock (Eq,Show)
