{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( Text32(unText32)
  , Text64(unText64)
  , ListValidation
  , VError(..)
--  , withArraySequenceA
  , withArraySeqList
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Semigroup
import qualified Data.Text           as T
import qualified Data.Traversable    as TR
import           Data.Validation
import qualified Data.Vector         as V

data VError = MustNotBeEmpty T.Text
            | MustBeLessThanLength32 T.Text
            | MustBeLessThanLength64 T.Text
            deriving (Eq, Show)

-- Basic validated types with smart constructors
newtype Text32 = Text32 { unText32 :: T.Text } deriving (Eq, Show)

-- The following can be cleaned by extracting common code
newtype Text64 = Text64 { unText64 :: T.Text } deriving (Eq, Show)

-- aeson helper for [f a] -> f [a]
-- withArraySequenceA :: (FromJSON (f a), Applicative f) => String -> Value -> Parser (f [a])
-- withArraySequenceA s = withArray s $ fmap TR.sequenceA . mapM parseJSON . V.toList

withArraySeqList s = withArray s $ fmap TR.sequenceA . mapM parseJSON . V.toList

-- CHRIS TODO
type ListValidation a = AccValidation ListError a

data ListError = ListError
  { e_title :: Maybe [VError]
  , e_owner :: Maybe [VError]
  , e_items :: Maybe [VError]
  } deriving (Show)

instance Semigroup (ListError) where
  (<>) a b = ListError (e_title a <> e_title b)
                       (e_owner a <> e_owner b)
                       (e_items a <> e_items b)

instance FromJSON (ListValidation Bool) where
  parseJSON = withBool "Bool" $ pure . pure

instance FromJSON (ListValidation Text32) where
  parseJSON = withText "Text32" $ pure . lText32

instance FromJSON (ListValidation Text64) where
  parseJSON = withText "Text64" $ pure . lText64

lText32 :: T.Text -> ListValidation Text32
lText32 t
   | T.length t == 0   = _Failure # ListError (Just [MustNotBeEmpty t]) Nothing Nothing
   | T.length t <= 32  = _Success # Text32 t
   | otherwise         = _Failure # ListError (Just [MustBeLessThanLength32 t]) Nothing Nothing

lText64 :: T.Text -> ListValidation Text64
lText64 t
  | T.length t == 0   = _Failure # ListError Nothing (Just [MustNotBeEmpty t]) Nothing
  | T.length t <= 64  = _Success # Text64 t
  | otherwise         = _Failure # ListError Nothing (Just [MustBeLessThanLength64 t]) Nothing
