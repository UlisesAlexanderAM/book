module Book () where

import Data.Char (isAlpha, isSpace)
import Data.Maybe (mapMaybe)
import Numeric.Natural (Natural)

newtype Letters = Letters String

instance Show Letters where
  show (Letters c) = c

letterString :: String -> Letters
letterString = Letters . mapMaybe letter

letter :: Char -> Maybe Char
letter c =
  if isAlpha c || isSpace c
    then Just c
    else Nothing

newtype FirstName = FirstName Letters

instance Show FirstName where
  show (FirstName s) = show s

newtype LastName = LastName Letters

instance Show LastName where
  show (LastName s) = show s

newtype FullName = FullName (LastName, FirstName)

instance Show FullName where
  show (FullName (a, b)) = show a ++ ", " ++ show b

stringToFullName :: (String, String) -> FullName
stringToFullName (a, b) = FullName (LastName $ letterString a, FirstName $ letterString b)

data Person
  = PersonFullName FullName
  | Alias String
  | Anonymous

instance Show Person where
  show (PersonFullName (FullName (a, b))) = show a ++ ", " ++ show b
  show (Alias s) = s
  show Anonymous = "Anonymous"

newtype Title = Title String

instance Show Title where
  show (Title s) = s

newtype Language = Language [Letters]

instance Show Language where
  show (Language s) = show s

newtype NoPages = NoPages Natural

newtype NoWords = NoWords Natural

newtype Editorial = Editorial String

instance Show Editorial where
  show (Editorial s) = s

newtype Author = Author Person deriving (Show)

newtype Translator = Translator Person

instance Show Translator where
  show (Translator s) = show s

data BookFormat
  = Physical
  | Digital

instance Show BookFormat where
  show Physical = "Physical"
  show Digital = "Digital"

newtype Editor = Editor Person

instance Show Editor where
  show (Editor s) = show s

newtype Ilustrator = Ilustrator Person

instance Show Ilustrator where
  show (Ilustrator s) = show s

--TODO: create a tyoe for published date and publishing period
--TODO: decide if use a enum type or a type class for categories and genres
--TODO: decide how to aproach optionality before creating book type