{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module MarkLight.Types
  ( URLPath (..),
    LocalPath (..),
    TargetPath (..),
    Title (..),
    Author (..),
    Text (..),
    CSSID (..),
    ReadLocal (..),
    WriteLocal (..),
    LightAtom (..),
    LightBlock (..),
    PageInformation (..),
    Page (..),
    FlagIncludesMenu(..),
    FlagRegisterMenuEntry(..),
    Style (..),
  )
where

import Control.Monad
import Data.String
import qualified Data.Map as M
import Optics
import Prelude hiding (div, head, id)

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

newtype CSSID = MkID String deriving (Eq, Show)

newtype FlagIncludesMenu = MkIncMenuFlag Bool deriving (Eq, Show)
newtype FlagRegisterMenuEntry = MkRegisterMenuEntryFlag Bool deriving (Eq, Show)

data Style = NoStyle | StyleCentered deriving (Eq, Show)

data LightAtom
  = Word String
  | Newline
  | Space
  | Link URLPath Text
  | Book Title Author (Maybe URLPath)
  deriving (Eq, Show)

data LightBlock
  = Plain [LightBlock]
  | HFlex [LightBlock]
  | Para [LightAtom]
  | Direct [LightAtom]
  | Header [LightAtom]
  | Enumeration [LightBlock]
  | Preformated String
  | Picture URLPath Title CSSID Style
  | PublicationList LocalPath
  | Comment
  deriving (Eq, Show)

instance Semigroup LightBlock where
  (<>) (Plain a) (Plain b) = Plain (a <> b)
  (<>) a@(Plain _) b = a <> Plain [b]
  (<>) a b@(Plain _) = Plain [a] <> b
  (<>) a b = Plain [a, b]

instance Monoid LightBlock where
  mempty = Plain []

data PageInformation = MkPageInformation
  { getPageTitle :: Title,
    getPagePath :: TargetPath,
    getFlagIncludesMenu :: FlagIncludesMenu,
    getFlagAddEntry :: FlagRegisterMenuEntry
  }
  deriving (Show)

data Page = MkPage
    { getPageMetadata :: PageInformation,
      getContent :: LightBlock } deriving (Show)

class (Monad m) => ReadLocal m where
  readResource :: LocalPath -> m String

class (Monad m) => WriteLocal m where
    writeResource :: LocalPath -> String -> m ()
