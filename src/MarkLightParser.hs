{-# LANGUAGE OverloadedStrings #-}

module MarkLightParser
  ( interpretMarkLight,
    parseMarkLight,
    LocalPath(..),
    Page,
    LightAtom(..),
    LightBlock(..),
    parseParagraph,
    parseLink,
    parseArguments
  )
where

import Control.Monad
import qualified Data.Map as M
import qualified Text.Parsec as P
import Text.Parsec
  ( (<?>),
    (<|>),
    char,
    eof,
    letter,
    many,
    many1,
    manyTill,
    parse,
    sourceColumn,
    space,
    spaces,
    string,
    try,
  )
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String
import qualified Utils as U
import Prelude hiding (div, head, id)

import MarkLight.Arguments

newtype URLPath = MkURLPath String deriving (Eq, Show)

newtype LocalPath = MkLocalPath String deriving (Show)

newtype TargetPath = MkTargetPath String deriving (Show)

newtype Title = MkTitle String deriving (Eq, Show)

newtype Author = MkAuthor String deriving (Eq, Show)

newtype Text = MkText String deriving (Eq, Show)

data FileSource = MkFileSource LocalPath String deriving (Show)

data LightAtom
  = Word String
  | Newline
  | Space
  | Picture URLPath Title
  | Link URLPath Text
  | Book Title Author
  deriving (Eq, Show)

data LightBlock
  = Plain [LightBlock]
  | HFlex [LightBlock]
  | Para [LightAtom]
  | Direct [LightAtom]
  | Header [LightAtom]
  | Enumeration [LightBlock]
  | Comment
  deriving (Eq, Show)

instance Semigroup LightBlock where
  (<>) (Plain a) (Plain b) = Plain (a <> b)
  (<>) a@(Plain _) b = a <> Plain [b]
  (<>) a b@(Plain _) = Plain [a] <> b
  (<>) a b = Plain [a, b]

instance Monoid LightBlock where
  mempty = Plain []

data PageInformation = MkPageInformation Title TargetPath deriving (Show)

data Page = MkPage PageInformation LightBlock deriving (Show)

-- Tokenized character sets for the various areas
urlLetters = tokenize $ many1 (alphaNum <|> char ':' <|> char '/' <|> char '.' <|> char '-')

linkLetters = tokenize $ many1 (alphaNum <|> char ' ' <|> char '.')

wordLetter = alphaNum <|> char '.' <|> char ':' <|> char '!' <|> char '?' <|> char ',' <|> char ')' <|> char '('

charToken a = tokenize $ char a

stringToken str = tokenize $ string str

braceCommand str p = between
      (charToken '{')
      (char '}')
      (stringToken str >> p)

failIfAbsent :: MonadFail m => Maybe a -> String -> m a
failIfAbsent (Just x) _ = return x
failIfAbsent Nothing str = fail str

extractPageInformation :: MonadFail m => Arguments -> m PageInformation
extractPageInformation args =
  MkPageInformation
    <$> failIfAbsent (MkTitle <$> getArgument "title" args) "Could not retrieve title="
    <*> failIfAbsent (MkTargetPath <$> getArgument "path" args) "Could not retrieve path="

ensureStartOfLine :: Monad m => ParsecT s u m ()
ensureStartOfLine = do
  pos <- getPosition
  guard (sourceColumn pos == 1)

preventStartOfLine :: Monad m => ParsecT s u m ()
preventStartOfLine = do
  pos <- getPosition
  guard (sourceColumn pos /= 1)

-- inlineSpace = (preventStartOfLine >> space) <|> char ' ' <|> tab

blank = char ' '
nonNewlineSpace = blank <|> tab
spacesWithAtMostOneNewline = do
    many nonNewlineSpace
    optional (newline >> (many nonNewlineSpace))

tokenize :: Parser a -> Parser a
tokenize p = p <* optional spacesWithAtMostOneNewline

tokenizeBlock :: Parser a -> Parser a
tokenizeBlock p = p <* spaces

parseSpace :: Parser LightAtom
parseSpace = tokenize ( (lookAhead space) >> return Space)

parseWord :: Parser LightAtom
parseWord = Word <$> (many1 wordLetter)

parseLinebreak :: Parser LightAtom
parseLinebreak = tokenize $ try (stringToken "<br>" >> (return Newline))

optionalSepBy :: Parser a -> Parser a -> Parser [a]
optionalSepBy sep p = do
    as <- many1 (helper sep p <|> return <$> p)
    return $ concat as
    where helper sep p = try $ do
            ss <- sep
            ps <- p
            return (ss:ps:[])

parsePageInformation :: Parser PageInformation
parsePageInformation = tokenizeBlock $ braceCommand "page" $ do
  opts <- parseArgumentsWithDefaults ["path","title"]
  extractPageInformation opts

parseLink :: Parser LightAtom
parseLink = braceCommand "link" $ do
    opts <- parseArgumentsWithDefaults ["path", "text"]
    case Link
        <$> (MkURLPath <$> getArgument "path" opts)
        <*> (MkText <$> getArgument "text" opts) of
        Nothing -> fail "Arguments for link not present"
        Just lnk -> return lnk

parsePicture :: Parser LightAtom
parsePicture = braceCommand "picture" $ do
  opts <- parseArgumentsWithDefaults ["path", "title"]
  case Picture
    <$> (MkURLPath <$> getArgument "path" opts)
    <*> (MkTitle <$> getArgument "title" opts) of
    Nothing -> fail "Arguments for picture not present"
    Just pic -> return pic

parseBook :: Parser LightAtom
parseBook = braceCommand "book" $ do
  opts <- parseArgumentsWithDefaults ["title", "author"]
  case Book
    <$> (MkTitle <$> getArgument "title" opts)
    <*> (MkAuthor <$> getArgument "author" opts) of
    Nothing -> fail "Arguments for author not present"
    Just bk -> return bk


parseHeader :: Parser LightBlock
parseHeader = tokenizeBlock $ ensureStartOfLine *> do
  charToken '='
  hdr <- many1 parseWord
  return $ Header hdr

removeRedundantSpaces :: [LightAtom] -> [LightAtom]
removeRedundantSpaces [] = []
removeRedundantSpaces (Space:xs) = removeRedundantSpaces xs
removeRedundantSpaces (x:Space:Space:xs) = removeRedundantSpaces (x:Space:xs)
removeRedundantSpaces (x:Space:y:xs) = x:Space:removeRedundantSpaces (y:xs)
removeRedundantSpaces (x:xs) = x:removeRedundantSpaces xs

parseParagraph :: Parser LightBlock
parseParagraph = tokenizeBlock $ do
  text <- optionalSepBy parseSpace (parseLinebreak <|> parseWord <|> try parseLink <|> try parsePicture <|> try parseBook)
  return $ Para $ removeRedundantSpaces text

parseDirect :: Parser LightBlock
parseDirect = tokenizeBlock $ do
  text <- optionalSepBy parseSpace (parseLinebreak <|> parseWord <|> try parseLink <|> try parsePicture <|> try parseBook)
  return $ Direct $ removeRedundantSpaces text


parseEnumItem :: Parser LightBlock
parseEnumItem = tokenize $ ensureStartOfLine *> do
    charToken '-'
    block <- parseDirect
    return block

parseEnumeration :: Parser LightBlock
parseEnumeration = tokenizeBlock $ ensureStartOfLine *> do
    blocks <- many1 parseEnumItem
    return $ Enumeration blocks

parseComment :: Parser LightBlock
parseComment = tokenizeBlock $ ensureStartOfLine *> do
    charToken '#'
    manyTill anyChar (newline) >> return Comment

parseBlock :: Parser LightBlock
parseBlock = parseEnumeration <|> parseHeader <|> parseParagraph <|> parseComment

parsePage :: Parser Page
parsePage = do
  pageInformation <- parsePageInformation
  elms <- many parseBlock
  eof
  return $ MkPage pageInformation $ mconcat elms

parseMarkLight :: MonadFail m => LocalPath -> String -> m Page
parseMarkLight (MkLocalPath path) cont = case parse parsePage path cont of
    Left err -> fail $ show err
    Right page -> return page

interpretMarkLight :: Page -> U.Html
interpretMarkLight (MkPage pageinfo lightblock) = U.page (renderTitle pageinfo) $ do
    -- U.menuBlock
    U.pageTitle (renderTitle pageinfo)
    renderLightBlock lightblock

renderTitle :: PageInformation -> U.Html
renderTitle (MkPageInformation (MkTitle title) _) = U.toHtml title

renderLightBlock :: LightBlock -> U.Html
renderLightBlock (Header las) = U.headline (renderLightAtomList las)
renderLightBlock (Plain lbs) = mconcat (map renderLightBlock lbs)
renderLightBlock (Para las) = U.p $ (renderLightAtomList las)
renderLightBlock (Direct las) = renderLightAtomList las
renderLightBlock (Enumeration las) = U.ul $ mconcat $ U.li <$> (renderLightBlock <$> las)
renderLightBlock Comment = mempty

renderLightAtomList :: [LightAtom] -> U.Html
renderLightAtomList las = mconcat $ renderLightAtom <$> las

renderLightAtom :: LightAtom -> U.Html
renderLightAtom (Word str) = U.toHtml str
renderLightAtom (Link (MkURLPath path) (MkText txt)) = U.link path txt
renderLightAtom Space = U.toHtml (" " :: String)
renderLightAtom Newline = U.br
renderLightAtom (Picture (MkURLPath path) (MkTitle title)) = U.toHtml ("A picture" :: String)
renderLightAtom (Book (MkTitle title) (MkAuthor author)) = (U.em $ U.toHtml $ title) <> (U.toHtml $ " by " ++ author)
