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
newtype Name where
  Name :: NonEmpty Text -> Name
  deriving newtype (Eq, Show)

type Author :: Type
newtype Author where
  Author :: NonEmpty Name -> Author
  deriving newtype (Eq, Show)

type Translator :: Type
newtype Translator where
  Translator :: NonEmpty Name -> Translator
  deriving newtype (Eq, Show)

type Editor :: Type
newtype Editor where
  Editor :: NonEmpty Name -> Editor
  deriving newtype (Eq, Show)

type Illustrator :: Type
newtype Illustrator where
  Illustrator :: NonEmpty Name -> Illustrator
  deriving newtype (Eq, Show)

type Title :: Type
newtype Title where
  Title :: NonEmpty Text -> Title
  deriving newtype (Eq, Show)

type AlternativeTitle :: Type
newtype AlternativeTitle where
  AlternativeTitle :: NonEmpty Title -> AlternativeTitle
  deriving newtype (Eq, Show)

type Publisher :: Type
newtype Publisher where
  Publisher :: NonEmpty Text -> Publisher
  deriving newtype (Eq, Show)

type OriginalLanguage :: Type
newtype OriginalLanguage where
  OriginalLanguage :: NonEmpty Text -> OriginalLanguage
  deriving newtype (Eq, Show)

type BookLanguage :: Type
newtype BookLanguage where
  BookLanguage :: NonEmpty Text -> BookLanguage
  deriving newtype (Eq, Show)

type NumPages :: Type
newtype NumPages where
  NumPages :: Natural -> NumPages
  deriving newtype (Eq, Show)

type NumWords :: Type
newtype NumWords where
  NumWords :: Natural -> NumWords
  deriving newtype (Eq, Show)

type PubDate :: Type
newtype PubDate where
  PubDate :: Calendar.Day -> PubDate
  deriving newtype (Eq, Show)

type PubPeriod :: Type
newtype PubPeriod where
  PubPeriod :: Calendar.CalendarDiffDays -> PubPeriod
  deriving newtype (Eq, Show)

type PeriodSinceLastPub :: Type
newtype PeriodSinceLastPub where
  PeriodSinceLastPub ::
    Calendar.CalendarDiffDays ->
    PeriodSinceLastPub
  deriving newtype (Eq, Show)

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
data PublicationFormat where
  Physical :: PublicationFormat
  Digital :: PublicationFormat
  deriving stock (Eq, Show)

type ReadingStatus :: Type
data ReadingStatus where
  Reading :: ReadingStatus
  Read :: ReadingStatus
  WantedToRead :: ReadingStatus
  Pause :: ReadingStatus
  Dropped :: ReadingStatus
  deriving stock (Eq, Show)

type PublishingStatus :: Type
data PublishingStatus where
  Publishing :: PublishingStatus
  Finished :: PublishingStatus
  Hiatus :: PublishingStatus
  Cancelled :: PublishingStatus
  deriving stock (Eq, Show)

type PeriodicalPeriod :: Type
data PeriodicalPeriod where
  Weekly :: PeriodicalPeriod
  Biweekly :: PeriodicalPeriod
  Monthly :: PeriodicalPeriod
  Bimonthly :: PeriodicalPeriod
  Quaterly :: PeriodicalPeriod
  Annually :: PeriodicalPeriod
  deriving stock (Eq, Show)

-- * Book computations

mkName :: NonEmpty Text -> Name
mkName = Name

-- mkAlternativeTitle :: NonEmpty Text -> AlternativeTitle
-- mkAlternativeTitle = AlternativeTitle . nonEmpty ([Title])

-- * Actions: How we interact with the world?
