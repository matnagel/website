{-# LANGUAGE OverloadedStrings #-}

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
    IsValue (..),
    Value (..),
    LightAtom (..),
    LightBlock (..),
    PageInformation (..),
    Page (..),
    FlagIncludesMenu(..),
    FlagRegisterMenuEntry(..),
    Style (..)
  )
where

import Control.Monad
import Data.String
import qualified Data.Map as M
import Optics
import Prelude hiding (div, head, id)

data Value = MkValue String | MkBool Bool

instance Show Value where
  show (MkValue str) = show str
  show (MkBool a) = show a

class IsValue a where
  fromValue :: MonadFail m => Value -> m a

newtype URLPath = MkURLPath String deriving (Eq, Show)

newtype LocalPath = MkLocalPath String deriving (Eq, Show)

instance IsString LocalPath where
    fromString path = MkLocalPath path

newtype TargetPath = MkTargetPath String deriving (Show)

newtype Title = MkTitle String deriving (Eq, Show)

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

instance IsValue URLPath where
  fromValue (MkValue val) = return $ MkURLPath val

instance IsValue LocalPath where
  fromValue (MkValue val) = return $ MkLocalPath val

instance IsValue Title where
  fromValue (MkValue val) = return $ MkTitle val

instance IsString Title where
  fromString str = MkTitle str

instance IsValue TargetPath where
  fromValue (MkValue val) = return $ MkTargetPath val

instance IsString TargetPath where
  fromString str = MkTargetPath str

instance IsValue Text where
  fromValue (MkValue val) = return $ MkText val

instance IsValue CSSID where
  fromValue (MkValue val) = return $ MkID val

instance IsValue Author where
  fromValue (MkValue val) = return $ MkAuthor val

instance IsValue Style where
  fromValue (MkValue "centered") = return $ StyleCentered
  fromValue _ = fail "Unknown Style"


instance IsValue FlagIncludesMenu where
  fromValue (MkValue "true") = return $ MkIncMenuFlag True
  fromValue (MkValue "false") = return $ MkIncMenuFlag False
  fromValue (MkBool a) = return $ MkIncMenuFlag a
  fromValue (MkValue _) = fail "Flag IncludesMenu needs to be either true or false"

instance IsValue FlagRegisterMenuEntry where
  fromValue (MkValue "true") = return $ MkRegisterMenuEntryFlag True
  fromValue (MkValue "false") = return $ MkRegisterMenuEntryFlag False
  fromValue (MkBool a) = return $ MkRegisterMenuEntryFlag a
  fromValue (MkValue _) = fail "Flag AddMenu needs to be either true or false"

class (Monad m) => ReadLocal m where
  readResource :: LocalPath -> m String

class (Monad m) => WriteLocal m where
    writeResource :: LocalPath -> String -> m ()

