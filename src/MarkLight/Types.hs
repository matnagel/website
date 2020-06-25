{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module MarkLight.Types
  (
    LightAtom (..),
    LightBlock (..),
    PageInformation (..),
    Page (..),
    FlagIncludesMenu(..),
    FlagRegisterMenuEntry(..),
    Style (..),
  )
where

import Types

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
  | Header Text
  | Enumeration [[LightAtom]]
  | Preformated String
  | Picture URLPath Title PictureSize Style
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
