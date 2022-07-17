{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Book
  (
    -- * Book classes
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
    Authors,
    Translator (..),
    Translators,
    Editor (..),
    Editors,
    Illustrator (..),
    Ilustrators,
    Title (..),
    AlternativeTitle,
    AlternativeTitles,
    Publisher (..),
    Language (..),
    NumPages (..),
    NumWords (..),
    PubDate (..),
    PubPeriod (..),
    PeriodSinceLastPub (..),
    ReadingStatus (..),
    PublishingStatus (..),
    SimpleBook(..),

    -- * Custom constructors
    mkPersonAlias,
    mkPersonName,
    mkAlternativeTitle
  )
where

import qualified Data.Time.Calendar as Calendar (CalendarDiffDays, Day)
import qualified Numeric.Natural as Nat (Natural)
import Data.List.NonEmpty (NonEmpty)

class Book a where
  changeReadingStatus :: a  -> ReadingStatus -> a
  changePubPeriod :: a -> PubPeriod -> a
  updatePeriodSinceLastPub :: a -> PeriodSinceLastPub -> a
  updatePublishingStatus :: a -> PublishingStatus -> a

class Book a => BookFormat a where
  hasPhysical :: a -> Bool -> Bool
  hasDigital :: a -> Bool -> Bool

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

type Authors = NonEmpty [Author]

newtype Translator = Translator Person deriving newtype (Eq, Show)

type Translators = [Translator]

newtype Editor = Editor Person deriving newtype (Eq, Show)

type Editors = [Editor]

newtype Illustrator = Illustrator Person deriving newtype (Eq, Show)

type Ilustrators = [Illustrator]

newtype Title = Title String deriving newtype (Eq, Show)

newtype AlternativeTitle = AlternativeTitle Title deriving newtype (Eq, Show)

type AlternativeTitles = [AlternativeTitle]

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

data SimpleBook = SimpleBook
  {
  title :: Title,
  authors :: Authors,
  publisher :: Publisher,
  language :: Language,
  numPages :: NumPages,
  pubDate :: PubDate,
  pubPeriod :: PubPeriod,
  periodSinceLastPub :: PeriodSinceLastPub,
  publishingStatus :: PublishingStatus,
  readingStatus :: ReadingStatus
  }

instance Book SimpleBook where
  changeReadingStatus :: SimpleBook -> ReadingStatus -> SimpleBook
  changeReadingStatus book status = book {readingStatus = status}
  changePubPeriod :: SimpleBook -> PubPeriod -> SimpleBook
  changePubPeriod book period = book {pubPeriod = period}
  updatePublishingStatus :: SimpleBook -> PublishingStatus -> SimpleBook
  updatePublishingStatus book status = book {publishingStatus = status}
  updatePeriodSinceLastPub :: SimpleBook -> PeriodSinceLastPub -> SimpleBook
  updatePeriodSinceLastPub book lastPub = book {periodSinceLastPub = lastPub}
