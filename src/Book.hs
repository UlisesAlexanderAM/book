module Book
  ( -- * Publication classes
    Publication (..),
    Collection (..),
    Serial (..),
    Periodical (..),
    Genre (..),
    BookCategory (..),
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
    Illustrators,
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

    -- * Custom constructors
    mkPersonAlias,
    mkPersonName,
    mkAlternativeTitle,
  )
where

import Data.Kind (Constraint, Type)
import Data.Time.Calendar qualified as Calendar (CalendarDiffDays, Day)
import Numeric.Natural qualified as Nat (Natural)
import RIO
import RIO.NonEmpty qualified as NE

type Publication :: Type -> Constraint
class Publication a where
  getPublicationTitle :: a -> Text
  changeReadingStatus :: a -> ReadingStatus -> a
  updatePeriodSinceLastPub :: a -> PeriodSinceLastPub -> a

type Collection :: Type -> Constraint
class Publication a => Collection a where
  getCollectionTitle :: a -> Text
  getCollectionTitle = getPublicationTitle

type Serial :: Type -> Constraint
class Publication a => Serial a where
  getSeriesTitle :: a -> Text
  getSeriesTitle = getPublicationTitle
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

type Person :: Type
data Person
  = PersonName Name
  | Alias Text
  | Anonymous
  deriving stock (Eq, Show)

mkPersonName :: Text -> Person
mkPersonName = PersonName . Name

mkPersonAlias :: Text -> Person
mkPersonAlias = Alias

type Author :: Type
newtype Author = Author Person deriving newtype (Eq, Show)

type Authors :: Type
type Authors = NE.NonEmpty [Author]

type Translator :: Type
newtype Translator = Translator Person deriving newtype (Eq, Show)

type Translators :: Type
type Translators = [Translator]

type Editor :: Type
newtype Editor = Editor Person deriving newtype (Eq, Show)

type Editors :: Type
type Editors = [Editor]

type Illustrator :: Type
newtype Illustrator = Illustrator Person deriving newtype (Eq, Show)

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

type PublicationLanguage :: Type
newtype PublicationLanguage = PublicationLanguage Text deriving newtype (Eq, Show)

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
