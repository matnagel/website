{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Types
  ( URLPath (..),
    LocalPath (..),
    TargetPath (..),
    Title (..),
    Author (..),
    Text (..),
    ReadLocal (..),
    WriteLocal (..),
    PictureSize (..)
  )
where

import Data.String

newtype URLPath = MkURLPath String deriving (Eq, Show)

newtype LocalPath = MkLocalPath String deriving (Eq, Show)

instance IsString LocalPath where
    fromString path = MkLocalPath path

newtype TargetPath = MkTargetPath String deriving (Show)

instance IsString TargetPath where
    fromString str = MkTargetPath str

newtype Title = MkTitle String deriving (Eq, Show)

instance IsString Title where
    fromString str = MkTitle str

newtype Author = MkAuthor String deriving (Eq, Show)

newtype Text = MkText String deriving (Eq, Show)

data PictureSize = MkSizeHeight Float | MkSizeWidth Float deriving (Eq, Show)

class (Monad m) => ReadLocal m where
  readResource :: LocalPath -> m String

class (Monad m) => WriteLocal m where
    writeResource :: LocalPath -> String -> m ()
