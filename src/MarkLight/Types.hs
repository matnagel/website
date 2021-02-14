{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, StandaloneDeriving #-}

module MarkLight.Types
  (
    LightAtom (..),
    LightBlock (..),
    PageInformation (..),
    Page (..),
    FlagIncludesMenu(..),
    FlagRegisterMenuEntry(..),
    Style (..)
  )
where

import Types
import HtmlInterface

newtype FlagIncludesMenu = MkIncMenuFlag Bool deriving (Eq, Show)
newtype FlagRegisterMenuEntry = MkRegisterMenuEntryFlag Bool deriving (Eq, Show)

data Style = NoStyle | StyleCentered | StyleRight deriving (Eq, Show)

data LightAtom where
  Word :: String -> LightAtom
  Newline :: LightAtom
  Space :: LightAtom
  Link :: URLPath -> Text -> LightAtom
  AtomDomain :: (ToCoreInlineElements a, Show a) =>  a -> LightAtom

deriving instance Show LightAtom

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
  | RightPicture LightBlock URLPath Title PictureSize
  | Comment
  deriving Show

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
