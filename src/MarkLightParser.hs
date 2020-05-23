{-# LANGUAGE OverloadedStrings #-}

module MarkLightParser
  ( interpretMarkLight,
    parseMarkLight,
    LocalPath(..),
    Page,
    LightAtom(..),
    LightBlock(..),
    parseParagraph
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

newtype URLPath = MkURLPath String deriving (Eq, Show)

newtype LocalPath = MkLocalPath String deriving (Show)

newtype TargetPath = MkTargetPath String deriving (Show)

newtype Title = MkTitle String deriving (Eq, Show)

newtype Text = MkText String deriving (Eq, Show)

newtype Options = MkOptions (M.Map String String) deriving (Show)

data FileSource = MkFileSource LocalPath String deriving (Show)

data LightAtom
  = Word String
  | Newline
  | Space
  | Picture URLPath Title
  | Link URLPath Text
  deriving (Eq, Show)

data LightBlock
  = Plain [LightBlock]
  | HFlex [LightBlock]
  | Para [LightAtom]
  | Header [LightAtom]
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

keyletters = tokenize $ many1 alphaNum

valueLetters = tokenize $ many1 (alphaNum <|> char ':' <|> char '/' <|> char ' ' <|> char '.')

wordLetter = alphaNum <|> char '.' <|> char ':' <|> char '!' <|> char '?' <|> char ','

charToken a = tokenize $ char a

stringToken str = tokenize $ string str

braceCommand str p = between
      (charToken '{')
      (char '}')
      (stringToken str >> p)

quote p = tokenize $ between (char '\"') (charToken '\"') p

failIfAbsent :: MonadFail m => Maybe a -> String -> m a
failIfAbsent (Just x) _ = return x
failIfAbsent Nothing str = fail str

extractPageInformation :: MonadFail m => Options -> m PageInformation
extractPageInformation (MkOptions omap) =
  MkPageInformation
    <$> failIfAbsent (MkTitle <$> M.lookup "title" omap) "Could not retrieve title="
    <*> failIfAbsent (MkTargetPath <$> M.lookup "path" omap) "Could not retrieve path="

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
spacesWithAtMostOneNewline = (many1 nonNewlineSpace) <|> (newline >> (many nonNewlineSpace))

tokenize :: Parser a -> Parser a
tokenize p = p <* spacesWithAtMostOneNewline

tokenizeBlock :: Parser a -> Parser a
tokenizeBlock p = p <* spaces

parseSpace :: Parser LightAtom
parseSpace = tokenize ( (lookAhead space) >> return Space)

parseWord :: Parser LightAtom
parseWord = Word <$> (many1 wordLetter)

-- emptyLine :: Parser LightAtom
-- emptyLine = (newline) *> (return Newline)

optionalSepBy :: Parser a -> Parser a -> Parser [a]
optionalSepBy sep p = do
    as <- many1 (helper sep p <|> return <$> p)
    return $ concat as
    where helper sep p = try $ do
            ss <- sep
            ps <- p
            return (ss:ps:[])

parseOptionsHelper :: Parser (String, String)
parseOptionsHelper = tokenize $ do
  key <- keyletters
  charToken '='
  value <- quote valueLetters
  return $ (key, value)

parseOptions :: Parser Options
parseOptions = tokenize $ do
  opts <- many parseOptionsHelper
  return $ MkOptions $ M.fromList opts

parsePageInformation :: Parser PageInformation
parsePageInformation = tokenizeBlock $ braceCommand "page" $ do
  opts <- parseOptions
  extractPageInformation opts

parseLink :: Parser LightAtom
parseLink = braceCommand "link" $ do
  url <- quote urlLetters
  name <- quote linkLetters
  return $ Link (MkURLPath url) (MkText name)

parseHeader :: Parser LightBlock
parseHeader = tokenizeBlock $ ensureStartOfLine *> do
  charToken '='
  hdr <- many1 parseWord
  return $ Header hdr

parseParagraph :: Parser LightBlock
parseParagraph = tokenizeBlock $ ensureStartOfLine *> do
  text <- optionalSepBy parseSpace (parseWord <|> parseLink)
  return $ Para $ removeRedundantSpaces text

removeRedundantSpaces :: [LightAtom] -> [LightAtom]
removeRedundantSpaces [] = []
removeRedundantSpaces (Space:xs) = removeRedundantSpaces xs
removeRedundantSpaces (x:Space:Space:xs) = removeRedundantSpaces (x:Space:xs)
removeRedundantSpaces (x:Space:y:xs) = x:Space:removeRedundantSpaces (y:xs)
removeRedundantSpaces (x:xs) = x:removeRedundantSpaces xs

parsePage :: Parser Page
parsePage = do
  pageInformation <- parsePageInformation
  elms <- many (parseHeader <|> parseParagraph)
  eof
  return $ MkPage pageInformation $ mconcat elms

parseMarkLight :: MonadFail m => LocalPath -> String -> m Page
parseMarkLight (MkLocalPath path) cont = case parse parsePage path cont of
    Left err -> fail $ show err
    Right page -> return page

interpretMarkLight :: Page -> U.Html
interpretMarkLight (MkPage pageinfo lightblock) = U.page (renderTitle pageinfo) $ do
    U.menuBlock
    U.pageTitle (renderTitle pageinfo)
    renderLightBlock lightblock

renderTitle :: PageInformation -> U.Html
renderTitle (MkPageInformation (MkTitle title) _) = U.toHtml title

renderLightBlock :: LightBlock -> U.Html
renderLightBlock (Header las) = U.headline (renderLightAtomList las)
renderLightBlock (Plain lbs) = mconcat (map renderLightBlock lbs)
renderLightBlock (Para las) = U.p $ (renderLightAtomList las)

renderLightAtomList :: [LightAtom] -> U.Html
renderLightAtomList las = mconcat $ renderLightAtom <$> las

renderLightAtom :: LightAtom -> U.Html
renderLightAtom (Word str) = U.toHtml str
renderLightAtom (Link (MkURLPath path) (MkText txt)) = U.link path txt
renderLightAtom Space = U.toHtml (" " :: String)
