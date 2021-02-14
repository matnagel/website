{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, StandaloneDeriving #-}

module Domain
  (
    Platform (..),
    Book (..),
    Course (..)
  )
where

import Text.Parsec ((<|>), string)

import Types
import HtmlInterface (ToCoreInlineElements)
import MarkLight.Arguments (Parseable(..), Argument(..), Argumentable(..))
import qualified HtmlInterface as HI

data Book = CBook Title Author (Maybe URLPath) deriving Show

instance Argumentable Book where
    stdArgument = CBook
        <$> FromKey "title" stdParser
        <*> FromKey "author" stdParser
        <*> FromKeyDefault "link" (return <$> stdParser) Nothing

instance ToCoreInlineElements Book where
    toCoreInlineElements (CBook (MkTitle title) (MkAuthor author) (Just path)) = [(HI.Em $ [HI.Link path (MkText title)]), (HI.Text $ " by " ++ author)]
    toCoreInlineElements (CBook (MkTitle title) (MkAuthor author) Nothing) = [(HI.Em $ [HI.Text $ title]), (HI.Text $ " by " ++ author)]

data Platform = Coursera | EDX deriving (Eq, Show)
instance Parseable Platform where
    stdParser = (string "Coursera" >> return Coursera)
        <|> (string "edX" >> return EDX)

data Course = CCourse Title Text Platform (Maybe URLPath) deriving Show

instance ToCoreInlineElements Course where
    toCoreInlineElements (CCourse (MkTitle title) (MkText desc) _ (Just path)) = [ (HI.Em $ [HI.Text $ title]), HI.Text $ " - " ++ desc, HI.Text ", ", HI.Link path $ MkText "certificate"]
    toCoreInlineElements (CCourse (MkTitle title) (MkText desc) _ Nothing) = [ (HI.Em $ [HI.Text $ title]), HI.Text $ " - " ++ desc]

instance Argumentable Course where
    stdArgument = CCourse
        <$> FromKey "title" stdParser
        <*> FromKey "desc" stdParser
        <*> FromKey "platform" stdParser
        <*> FromKeyDefault "link" (return <$> stdParser) Nothing

