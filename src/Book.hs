module Book
  ( -- * Book classes
    Book (..),
    Collection (..),
    Serial (..),
    Periodical (..),
    Genre (..),
    BookCategory (..),
    Tag (..),
    Isbn (..),
    Cover (..),

    -- * Book types
    Author (..),
    Authors,
    Translator (..),
    Translators,
    Editor (..),
    Editors,
    Illustrator (..),
    Illustrators,
    Title (..),
    AlternativeTitle,
    AlternativeTitles,
    Publisher (..),
    OriginalLanguage (..),
    BookLanguage (..),
    NumPages (..),
    NumWords (..),
    PubDate (..),
    PubPeriod (..),
    PeriodicalPeriod (..),
    PeriodSinceLastPub (..),
    PublicationFormat (..),
    ReadingStatus (..),
    PublishingStatus (..),

    -- * Custom constructors
    mkName,
    mkAlternativeTitle,

    -- * Show funtions

    -- textPersonName,
  )
where

import Data.Time.Calendar qualified as Calendar (CalendarDiffDays, Day)
import Universum

-- * Book data: What it is a book?

-- * Type classes

type Book :: Type -> Constraint
class Book a where
  getBookTitle :: a -> Text
  changeReadingStatus :: a -> ReadingStatus -> a
  updatePeriodSinceLastPub :: a -> PeriodSinceLastPub -> a

type Collection :: Type -> Constraint
class Book a => Collection a where
  getCollectionTitle :: a -> Text
  getCollectionTitle = getBookTitle

type Serial :: Type -> Constraint
class Book a => Serial a where
  getSeriesTitle :: a -> Text
  getSeriesTitle = getBookTitle
  updateVolumes :: a -> Int -> a
  changePubPeriod :: a -> PubPeriod -> a
  updatePublishingStatus :: a -> PublishingStatus -> a
  getVolume :: a -> Int

type Periodical :: Type -> Constraint
class Serial a => Periodical a where
  getPeriodicalTitle :: a -> Text
  getPeriodicalTitle = getSeriesTitle
  getPeriodicalPeriod :: a -> PeriodicalPeriod
  getIssue :: a -> Int

type Genre :: Type -> Constraint
class Genre a where
  getGenre :: a -> Text

type BookCategory :: Type -> Constraint
class BookCategory a where
  getBookCategory :: a -> Text

type Tag :: Type -> Constraint
class Tag a where
  getTag :: a -> Text

type Isbn :: Type -> Constraint
class Isbn a where
  mkIsbn :: Text -> a
  getIsbn :: a -> Text

type Cover :: Type -> Constraint
class Cover a where
  getCover :: a -> b
  mkCover :: b -> a

-- * Newtypes

type Name :: Type
newtype Name = Name (NonEmpty Text) deriving newtype (Eq, Show)

type Author :: Type
newtype Author = Author (NonEmpty Name) deriving newtype (Eq, Show)

type Translator :: Type
newtype Translator = Translator (NonEmpty Name) deriving newtype (Eq, Show)

type Editor :: Type
newtype Editor = Editor (NonEmpty Name) deriving newtype (Eq, Show)

type Illustrator :: Type
newtype Illustrator = Illustrator (NonEmpty Name) deriving newtype (Eq, Show)

type Title :: Type
newtype Title = Title (NonEmpty Text) deriving newtype (Eq, Show)

type AlternativeTitle :: Type
newtype AlternativeTitle = AlternativeTitle (NonEmpty Title) deriving newtype (Eq, Show)

type Publisher :: Type
newtype Publisher = Publisher (NonEmpty Text) deriving newtype (Eq, Show)

type OriginalLanguage :: Type
newtype OriginalLanguage = OriginalLanguage (NonEmpty Text) deriving newtype (Eq, Show)

type BookLanguage :: Type
newtype BookLanguage = PublicationLanguage (NonEmpty Text) deriving newtype (Eq, Show)

type NumPages :: Type
newtype NumPages = NumPages Natural deriving newtype (Eq, Show)

type NumWords :: Type
newtype NumWords = NumWords Natural deriving newtype (Eq, Show)

type PubDate :: Type
newtype PubDate = PubDate Calendar.Day deriving newtype (Eq, Show)

type PubPeriod :: Type
newtype PubPeriod = PubPeriod Calendar.CalendarDiffDays deriving newtype (Eq, Show)

type PeriodSinceLastPub :: Type
newtype PeriodSinceLastPub = PeriodSinceLastPub Calendar.CalendarDiffDays deriving newtype (Eq, Show)

-- * Type synonyms

type Authors :: Type
type Authors = NonEmpty [Author]

type Translators :: Type
type Translators = [Translator]

type Editors :: Type
type Editors = [Editor]

type Illustrators :: Type
type Illustrators = [Illustrator]

type AlternativeTitles :: Type
type AlternativeTitles = [AlternativeTitle]

-- * Data types

type PublicationFormat :: Type
data PublicationFormat
  = Physical
  | Digital
  deriving stock (Eq, Show)

type ReadingStatus :: Type
data ReadingStatus
  = Reading
  | Read
  | WantedToRead
  | Pause
  | Dropped
  deriving stock (Eq, Show)

type PublishingStatus :: Type
data PublishingStatus
  = Publishing
  | Finished
  | Hiatus
  | Cancelled
  deriving stock (Eq, Show)

type PeriodicalPeriod :: Type
data PeriodicalPeriod
  = Weekly
  | Biweekly
  | Monthly
  | Bimonthly
  | Quaterly
  | Annually
  deriving stock (Eq, Show)

-- * Book computations

mkName :: NonEmpty Text -> Name
mkName = Name

-- mkAlternativeTitle :: NonEmpty Text -> AlternativeTitle
-- mkAlternativeTitle = AlternativeTitle . nonEmpty ([Title])

-- * Actions: How we interact with the world?
