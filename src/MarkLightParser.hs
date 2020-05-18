{-# LANGUAGE OverloadedStrings #-}

module MarkLightParser
  (
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
import Utils
import Prelude hiding (div, head, id)

newtype URLPath = MkURLPath String deriving (Show)

newtype LocalPath = MkLocalPath String deriving (Show)

newtype Name = MkName String deriving (Show)

newtype Text = MkText String deriving (Show)

newtype Options = MkOptions (M.Map String String) deriving (Show)

data LightAtom
  = Word String
  | Newline
  | Picture URLPath Name
  | Link URLPath Text
  deriving (Show)

data LightBlock
  = Plain [LightBlock]
  | HFlex [LightBlock]
  | Para [LightAtom]
  | Header [LightAtom]
  deriving (Show)

instance Semigroup LightBlock where
  (<>) (Plain a) (Plain b) = Plain (a <> b)
  (<>) a@(Plain _) b = a <> Plain [b]
  (<>) a b@(Plain _) = Plain [a] <> b
  (<>) a b = Plain [a, b]

instance Monoid LightBlock where
  mempty = Plain []

data PageInformation = PageInformation
  { getName :: Name,
    getOutput :: LocalPath
  }
  deriving (Show)

data Page = Page PageInformation LightBlock deriving (Show)

maybeFail :: MonadFail m => Maybe a -> String -> m a
maybeFail (Just x) _ = return x
maybeFail Nothing str = fail str

getPageInformation :: MonadFail m => Options -> m PageInformation
getPageInformation (MkOptions omap) = PageInformation
    <$> maybeFail (MkName <$> M.lookup "name" omap) "Could not retrieve name"
    <*> maybeFail (MkLocalPath <$> M.lookup "path" omap) "Could not retrieve path"

ensureStartOfLine :: Monad m => ParsecT s u m ()
ensureStartOfLine = do
  pos <- getPosition
  guard (sourceColumn pos == 1)

preventStartOfLine :: Monad m => ParsecT s u m ()
preventStartOfLine = do
  pos <- getPosition
  guard (sourceColumn pos /= 1)

redundantSpace = (preventStartOfLine >> space) <|> char ' ' <|> tab

lexemeRedundant :: Parser a -> Parser a
lexemeRedundant p = p <* (many redundantSpace)

lexemeBlock :: Parser a -> Parser a
lexemeBlock p = p <* spaces

wordLetter = alphaNum <|> char '.' <|> char ':'

parseWord :: Parser LightAtom
parseWord = lexemeRedundant (Word <$> (many1 wordLetter))

emptyLine :: Parser LightAtom
emptyLine = (newline) *> (return Newline)

symbol a = lexemeRedundant $ char a

stringToken str = lexemeRedundant $ string str

urlLetters = lexemeRedundant $ many1 (alphaNum <|> char ':' <|> char '/')

linkLetters = lexemeRedundant $ many1 (alphaNum <|> char ' ')

keyletters = lexemeRedundant $ many1 alphaNum

valueLetters = lexemeRedundant $ many1 (alphaNum <|> char ':' <|> char '/' <|> char ' ')

braceCommand str p = lexemeRedundant $ between (symbol '{') (symbol '}') (stringToken str >> p)

parseOptionsHelper :: Parser (String, String)
parseOptionsHelper = lexemeRedundant $ do
  key <- keyletters
  symbol '='
  value <- between (symbol '\"') (symbol '\"') valueLetters
  return $ (key, value)

parseOptions :: Parser Options
parseOptions = lexemeRedundant $ do
  opts <- many parseOptionsHelper
  return $ MkOptions $ M.fromList opts

parsePageInformation :: Parser PageInformation
parsePageInformation = braceCommand "page" $ do
  opts <- parseOptions
  getPageInformation opts

parseLink :: Parser LightAtom
parseLink = braceCommand "link" $ do
  url <- between (symbol '\"') (symbol '\"') urlLetters
  name <- between (symbol '\"') (symbol '\"') linkLetters
  return $ Link (MkURLPath url) (MkText name)

parseHeader :: Parser LightBlock
parseHeader = ensureStartOfLine *> do
  symbol '='
  hdr <- many1 parseWord
  return $ Header hdr

parseParagraph :: Parser LightBlock
parseParagraph = ensureStartOfLine *> do
  text <- many1 (parseWord <|> parseLink)
  return $ Para text

parsePage :: Parser Page
parsePage = do
  pageInformation <- parsePageInformation
  elms <- many $ lexemeBlock (parseHeader <|> parseParagraph)
  eof
  return $ Page pageInformation $ mconcat elms
