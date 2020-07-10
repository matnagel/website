{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module BibliographyGenerator (
    generateBibliography
) where

import Data.Aeson
import Data.ByteString.Lazy.Char8 (pack)
import Control.Monad ( mzero )
import HtmlInterface
import Utility.Separators

import Types
import Optics

data Journal = Journal { jname::String,
                         jvol::String } deriving (Show)

data Arxiv = Arxiv String deriving (Show)

renderJournal :: Journal -> CoreInline
renderJournal j = [Text (jname j), Space, Text (jvol j)]

instance FromJSON Journal where
    parseJSON (Object v) = Journal <$>
                v .: "Name" <*>
                v .: "Volume"
    parseJSON _ = mzero

data Publication = Pub {
                coauthors::Maybe [String],
                journal::Maybe Journal,
                arxiv::Maybe Arxiv,
                title::String,
                date::String,
                note::Maybe String
            }   deriving (Show)


instance FromJSON Publication where
    parseJSON (Object v) = Pub
                   <$>  v .:? "Coauthors"
                   <*>  v .:? "Journal"
                   <*>  v .:? "Arxiv"
                   <*>  v .:  "Title"
                   <*>  v .:  "Date"
                   <*>  v .:? "Note"
    parseJSON _ = mzero

instance FromJSON Arxiv where
    parseJSON v = Arxiv <$> parseJSON v

renderArxiv :: Arxiv -> CoreInline
renderArxiv (Arxiv str) = [Link (MkURLPath url) (MkText str)]
                where url = "https://arxiv.org/abs/" <> str

renderTitle :: String -> [CoreInlineElement]
renderTitle str = [Em $ [Text str]]

renderCoauthor :: [String] -> CoreInline
renderCoauthor xs = (Text "with ") : coAuthorList
       where coAuthorList = flatten $ (\x -> separatedEntry [(Text x)] [Text ", "]) <$> xs


renderPublication :: Publication -> CoreInline
renderPublication pub = flatten [sTitle, sCoauthor, sJournal, sArxiv, sDate, sNote]
     where  sCoauthor = separatedEntryMaybe (renderCoauthor <$> coauthors pub) [Newline]
            sTitle = separatedEntry (renderTitle $ title pub) [Newline]
            sJournal = separatedEntryMaybe (renderJournal <$> journal pub) [Text ", "]
            sArxiv = separatedEntryMaybe (renderArxiv <$> arxiv pub) [Text ", "]
            sDate = separatedEntry [Text $ date pub] [Text ", "]
            sNote = separatedEntryMaybe (return <$> Text <$> note pub) [Text ", "]

renderPublications :: [Publication] -> CoreHtml
renderPublications list = mconcat $ Div css <$> (Direct <$> map renderPublication list)
    where css = set cssClass $ Just "publication"

parseBibliography :: String -> [Publication]
parseBibliography x = case (eitherDecode $ pack x) of
            Right a -> a
            Left str -> error $ "JSON: " <> str

generateBibliography :: String -> CoreHtml
generateBibliography path = renderPublications $ parseBibliography path
