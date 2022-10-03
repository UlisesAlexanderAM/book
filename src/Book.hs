{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Book
  ( -- * Publication classes
    Publication (..),
    Collection (..),
    Serial (..),
    Periodical (..),
    Genre (..),
    Category (..),
    Tag (..),
    Isbn (..),
    Cover (..),

    -- * Publication types
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
    OriginalLanguage (..),
    PublicationLanguage (..),
    NumPages (..),
    NumWords (..),
    PubDate (..),
    PubPeriod (..),
    PeriodicalPeriod (..),
    PeriodSinceLastPub (..),
    PublicationFormat (..),
    ReadingStatus (..),
    PublishingStatus (..),
    SimpleBook (..),

    -- * Custom constructors
    mkPersonAlias,
    mkPersonName,
    mkAlternativeTitle,
  )
where

import qualified Data.List.NonEmpty as NE (NonEmpty)
import qualified Data.Time.Calendar as Calendar (CalendarDiffDays, Day)
import qualified Numeric.Natural as Nat (Natural)

class Publication a where
  getPublicationTitle :: a -> String
  changeReadingStatus :: a -> ReadingStatus -> a
  updatePeriodSinceLastPub :: a -> PeriodSinceLastPub -> a

class Publication a => Collection a where
  getCollectionTitle :: a -> String
  getCollectionTitle = getPublicationTitle

class Publication a => Serial a where
  getSeriesTitle :: a -> String
  getSeriesTitle = getPublicationTitle
  updateVolumes :: a -> Int -> a
  changePubPeriod :: a -> PubPeriod -> a
  updatePublishingStatus :: a -> PublishingStatus -> a
  getVolume :: a -> Int


class Serial a => Periodical a where
  getPeriodicalTitle :: a -> String
  getPeriodicalTitle = getSeriesTitle
  getPeriodicalPeriod :: a -> PeriodicalPeriod
  getIssue :: a -> Int


class Genre a where
  getGenre :: a -> String

class Category a where
  getCategory :: a -> String

class Tag a where
  getTag :: a -> String

class Isbn a where
  mkIsbn :: String -> a
  getIsbn :: a -> String

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

type Authors = NE.NonEmpty [Author]

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

newtype OriginalLanguage = OriginalLanguage String deriving newtype (Eq, Show)

newtype PublicationLanguage = PublicationLanguage String deriving newtype (Eq, Show)

newtype NumPages = NumPages Nat.Natural deriving newtype (Eq, Show)

newtype NumWords = NumWords Nat.Natural deriving newtype (Eq, Show)

newtype PubDate = PubDate Calendar.Day deriving newtype (Eq, Show)

newtype PubPeriod = PubPeriod Calendar.CalendarDiffDays deriving newtype (Eq, Show)

newtype PeriodSinceLastPub = PeriodSinceLastPub Calendar.CalendarDiffDays deriving newtype (Eq, Show)

data PublicationFormat
  = Physical
  | Digital
  deriving stock (Eq, Show)

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
  deriving stock (Eq, Show)

data PeriodicalPeriod
  = Weekly
  | Biweekly
  | Monthly
  | Bimonthly
  | Quaterly
  | Annually
  deriving stock (Eq, Show)

data SimpleBook = SimpleBook
  { title :: Title,
    authors :: Authors,
    publisher :: Publisher,
    publicationLanguage :: PublicationLanguage,
    numPages :: NumPages,
    pubDate :: PubDate,
    periodSinceLastPub :: PeriodSinceLastPub,
    readingStatus :: ReadingStatus
  }

instance Publication SimpleBook where
  getPublicationTitle :: SimpleBook -> String
  getPublicationTitle = show . title

  changeReadingStatus :: SimpleBook -> ReadingStatus -> SimpleBook
  changeReadingStatus book status = book {readingStatus = status}

  updatePeriodSinceLastPub :: SimpleBook -> PeriodSinceLastPub -> SimpleBook
  updatePeriodSinceLastPub book lastPub = book {periodSinceLastPub = lastPub}
