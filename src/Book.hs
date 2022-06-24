module Book () where

import Data.Char (isAlpha, isSpace)
import Data.Maybe (mapMaybe)

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

stringToFirstName :: String -> FirstName
stringToFirstName = FirstName . letterString

newtype LastName = LastName Letters

instance Show LastName where
    show (LastName s) = show s

stringToLastName :: String -> LastName
stringToLastName = LastName . letterString

data FullName
  = OnlyName Letters
  | FullName (LastName, FirstName)
  | FullNameM (LastName, FirstName)

instance Show FullName where
    show (OnlyName a) = show a
    show (FullName (a,b)) = show a ++ ", " ++ show b
    show (FullNameM (a,b)) = show a ++ ", " ++ show b
    

data Person
  = Person FullName
  | Alias String
  | Anonymous
