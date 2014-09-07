{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Todo
  ( List
  , list
  , Item
  , item
  , Person
  , person
  ) where

import           Control.Applicative
import           Data.Aeson

import           Types

-- Todo list of items to finish
data List = List { title :: Text32
                 , owner :: Person
                 , items :: [Item]
                 } deriving (Eq, Show)

-- Item on the todo list
data Item = Item { description :: Text32
                 , completed   :: Bool
                 } deriving (Eq, Show)

-- Person
data Person = Person { name :: Text64
                     } deriving (Eq, Show)


-- Smart constructors
list :: ListValidation Text32
     -> ListValidation Person
     -> ListValidation [Item]
     -> ListValidation List
list lTitle lOwner lItems = List <$> lTitle <*> lOwner <*> lItems

item :: ListValidation Text32 -> ListValidation Bool -> ListValidation Item
item iTitle iCompleted = Item <$> iTitle <*> iCompleted

person :: ListValidation Text64 -> ListValidation Person
person pName = Person <$> pName

-- aeson instances
instance FromJSON (ListValidation List) where
  parseJSON = withObject "List" $ \o ->
    list <$>
    o .: "title" <*>
    o .: "owner" <*>
    o .: "items"

instance FromJSON (ListValidation Person) where
  parseJSON = withObject "Person" $ \o ->
    person <$> o .: "name"

-- instance FromJSON (AppValidation Person) where
--   parseJSON = withObject "Person" $ \o ->
--     person <$> o .: "name"

instance FromJSON (ListValidation Item) where
  parseJSON = withObject "Item" $ \o ->
    item <$>
    o .: "description" <*>
    o .: "completed"

instance FromJSON (ListValidation [Item]) where
  parseJSON = withArraySeqList "[Item]"
