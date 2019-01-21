{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module BibliographyGenerator (
generateBibliography
) where

import Data.Aeson
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Control.Applicative ( (<*>), (<$>) )
import Data.Monoid ( mconcat, (<>), mempty )
import Control.Monad ( mzero, msum )
import Data.Traversable
import Utils
import Utility.Separators

data Journal = Journal { jname::String,
                         jvol::String } deriving (Show)

data Arxiv = Arxiv String deriving (Show)

renderJournal :: Journal -> Html
renderJournal j = toHtml (jname j) <> " " <> toHtml (jvol j)

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
                   <$>  v .:?  "Coauthors"
                   <*>  v .:? "Journal"
                   <*>  v .:? "Arxiv"
                   <*>  v .:  "Title"
                   <*>  v .:  "Date"
                   <*>  v .:? "Note"
    parseJSON _ = mzero

instance FromJSON Arxiv where
    parseJSON v = Arxiv <$> parseJSON v

renderArxiv :: Arxiv -> Html
renderArxiv (Arxiv str) =  link url str
                where url = "https://arxiv.org/abs/" <> str

renderTitle = em . toHtml

renderCoauthor :: [String] -> Html
renderCoauthor xs = "with " <> coAuthorList
       where coAuthorList = flatten $ (\x -> separatedEntry (toHtml x) ", ") <$> xs 

renderPublication :: Publication -> Html
renderPublication pub = divClass "publication"
  $ flatten [sTitle, sCoauthor, sJournal, sArxiv, sDate, sNote]
  where sCoauthor = separatedEntryMaybe (renderCoauthor <$> coauthors pub) br
        sTitle = separatedEntry (renderTitle $ title pub) br
        sJournal = separatedEntryMaybe (renderJournal <$> journal pub) ", "
        sArxiv = separatedEntryMaybe (renderArxiv <$> arxiv pub) ", "
        sDate = separatedEntry (toHtml $ date pub) ", "
        sNote = separatedEntryMaybe (toHtml <$> note pub) ", "

renderPublications :: [Publication] -> Html
renderPublications list = mconcat $ map renderPublication list

parseBibliography :: String -> [Publication]
parseBibliography x = case (eitherDecode $ pack x) of
            Right a -> a
            Left str -> error $ "JSON: " <> str

generateBibliography :: String -> Html
generateBibliography path = renderPublications $ parseBibliography path
