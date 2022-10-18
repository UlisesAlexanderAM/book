
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
import qualified RIO.Text as T
import RIO

class Publication a where
  getPublicationTitle :: a -> Text
  changeReadingStatus :: a -> ReadingStatus -> a
  updatePeriodSinceLastPub :: a -> PeriodSinceLastPub -> a

class Publication a => Collection a where
  getCollectionTitle :: a -> Text
  getCollectionTitle = getPublicationTitle

class Publication a => Serial a where
  getSeriesTitle :: a -> Text
  getSeriesTitle = getPublicationTitle
  updateVolumes :: a -> Int -> a
  changePubPeriod :: a -> PubPeriod -> a
  updatePublishingStatus :: a -> PublishingStatus -> a
  getVolume :: a -> Int


class Serial a => Periodical a where
  getPeriodicalTitle :: a -> Text
  getPeriodicalTitle = getSeriesTitle
  getPeriodicalPeriod :: a -> PeriodicalPeriod
  getIssue :: a -> Int


class Genre a where
  getGenre :: a -> Text

class Category a where
  getCategory :: a -> Text

class Tag a where
  getTag :: a -> Text

class Isbn a where
  mkIsbn :: Text -> a
  getIsbn :: a -> Text

class Cover a where
  getCover :: a -> b
  mkCover :: b -> a

newtype Name = Name Text deriving newtype (Eq, Show)

data Person
  = PersonName Name
  | Alias Text
  | Anonymous
  deriving stock (Eq, Show)

mkPersonName :: Text -> Person
mkPersonName = PersonName . Name

mkPersonAlias :: Text -> Person
mkPersonAlias = Alias

newtype Author = Author Person deriving newtype (Eq, Show)

type Authors = NE.NonEmpty [Author]

newtype Translator = Translator Person deriving newtype (Eq, Show)

type Translators = [Translator]

newtype Editor = Editor Person deriving newtype (Eq, Show)

type Editors = [Editor]

newtype Illustrator = Illustrator Person deriving newtype (Eq, Show)

type Ilustrators = [Illustrator]

newtype Title = Title Text deriving newtype (Eq, Show)

newtype AlternativeTitle = AlternativeTitle Title deriving newtype (Eq, Show)

type AlternativeTitles = [AlternativeTitle]

mkAlternativeTitle :: Text -> AlternativeTitle
mkAlternativeTitle = AlternativeTitle . Title

newtype Publisher = Publisher Text deriving newtype (Eq, Show)

newtype OriginalLanguage = OriginalLanguage Text deriving newtype (Eq, Show)

newtype PublicationLanguage = PublicationLanguage Text deriving newtype (Eq, Show)

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
  getPublicationTitle :: SimpleBook -> Text
  getPublicationTitle = show . title

  changeReadingStatus :: SimpleBook -> ReadingStatus -> SimpleBook
  changeReadingStatus book status = book {readingStatus = status}

  updatePeriodSinceLastPub :: SimpleBook -> PeriodSinceLastPub -> SimpleBook
  updatePeriodSinceLastPub book lastPub = book {periodSinceLastPub = lastPub}
