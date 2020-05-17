{-# LANGUAGE OverloadedStrings #-}

module MarkLightParser
  (
  )
where

import qualified Text.Parsec as P
import Text.Parsec
  ( (<?>),
    (<|>),
    char,
    eof,
    letter,
    many,
    many1,
    parse,
    space,
    spaces,
    string,
    manyTill,
    try,
    sourceColumn
  )
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String
import Utils
import Prelude hiding (div, head, id)

import Control.Monad

newtype URLPath = MkURLPath String deriving (Show)

newtype LocalPath = MkLocalPath String deriving (Show)

newtype Name = MkName String deriving (Show)

newtype Text = MkText String deriving (Show)

data LightAtom
  = Word String
  | Newline
  | Picture URLPath Name
  | Link URLPath Name
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
  (<>) a b = Plain [a,b]

instance Monoid LightBlock where
    mempty = Plain []

data PageInformation
  = PageInformation
      { getName :: Name,
        getURL :: URLPath
      } deriving (Show)

data Page = Page PageInformation LightBlock deriving (Show)

startOfLine :: Monad m => ParsecT s u m ()
startOfLine = do
    pos <- getPosition
    guard (sourceColumn pos == 1)

notAtStartOfLine :: Monad m => ParsecT s u m ()
notAtStartOfLine = do
    pos <- getPosition
    guard (sourceColumn pos /= 1)

redundantSpace = (notAtStartOfLine >> space) <|> char ' ' <|> tab

lexemeAtom :: Parser a -> Parser a
lexemeAtom p = p <* (many redundantSpace)

lexemeBlock :: Parser a -> Parser a
lexemeBlock p = p <* spaces

wordLetter = alphaNum <|> char '.' <|> char ':'

parseWord :: Parser LightAtom
parseWord = lexemeAtom (Word <$> (many1 wordLetter))

emptyLine :: Parser LightAtom
emptyLine = (newline) *> (return Newline)

parseHeader :: Parser LightBlock
parseHeader = startOfLine *> do
    lexemeAtom (char '=')
    hdr <- many1 parseWord
    return $ Header hdr

parseParagraph :: Parser LightBlock
parseParagraph = startOfLine *> do
    text <- many1 parseWord
    return $ Para text

parsePage :: Parser LightBlock
parsePage = do
    elms <- many $ lexemeBlock (parseHeader <|> parseParagraph)
    eof
    return $ mconcat elms
