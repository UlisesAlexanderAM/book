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

import Data.Kind (Constraint, Type)
import Data.Time.Calendar qualified as Calendar (CalendarDiffDays, Day)
import Numeric.Natural qualified as Nat (Natural)
import RIO (Eq, Int, Show, Text, (.))
import RIO.NonEmpty qualified as NE

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

type Name :: Type
newtype Name = Name Text deriving newtype (Eq, Show)

mkName :: Text -> Name
mkName = Name

type Author :: Type
newtype Author = Author Name deriving newtype (Eq, Show)

type Authors :: Type
type Authors = NE.NonEmpty [Author]

type Translator :: Type
newtype Translator = Translator Name deriving newtype (Eq, Show)

type Translators :: Type
type Translators = [Translator]

type Editor :: Type
newtype Editor = Editor Name deriving newtype (Eq, Show)

type Editors :: Type
type Editors = [Editor]

type Illustrator :: Type
newtype Illustrator = Illustrator Name deriving newtype (Eq, Show)

type Illustrators :: Type
type Illustrators = [Illustrator]

type Title :: Type
newtype Title = Title Text deriving newtype (Eq, Show)

type AlternativeTitle :: Type
newtype AlternativeTitle = AlternativeTitle Title deriving newtype (Eq, Show)

type AlternativeTitles :: Type
type AlternativeTitles = [AlternativeTitle]

mkAlternativeTitle :: Text -> AlternativeTitle
mkAlternativeTitle = AlternativeTitle . Title

type Publisher :: Type
newtype Publisher = Publisher Text deriving newtype (Eq, Show)

type OriginalLanguage :: Type
newtype OriginalLanguage = OriginalLanguage Text deriving newtype (Eq, Show)

type BookLanguage :: Type
newtype BookLanguage = PublicationLanguage Text deriving newtype (Eq, Show)

type NumPages :: Type
newtype NumPages = NumPages Nat.Natural deriving newtype (Eq, Show)

type NumWords :: Type
newtype NumWords = NumWords Nat.Natural deriving newtype (Eq, Show)

type PubDate :: Type
newtype PubDate = PubDate Calendar.Day deriving newtype (Eq, Show)

type PubPeriod :: Type
newtype PubPeriod = PubPeriod Calendar.CalendarDiffDays deriving newtype (Eq, Show)

type PeriodSinceLastPub :: Type
newtype PeriodSinceLastPub = PeriodSinceLastPub Calendar.CalendarDiffDays deriving newtype (Eq, Show)

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
