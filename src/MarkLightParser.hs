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
import qualified BibliographyGenerator as BG
import Prelude hiding (div, head, id)

import MarkLight.Arguments

newtype URLPath = MkURLPath String deriving (Eq, Show)

newtype LocalPath = MkLocalPath String deriving (Eq, Show)

newtype TargetPath = MkTargetPath String deriving (Show)

newtype Title = MkTitle String deriving (Eq, Show)

newtype Author = MkAuthor String deriving (Eq, Show)

newtype Text = MkText String deriving (Eq, Show)

newtype CSSID = MkID String deriving (Eq, Show)

newtype MenuInformation = MkMenu Bool deriving Show

instance IsValue URLPath where
    fromValue (MkValue val) = return $ MkURLPath val

instance IsValue LocalPath where
    fromValue (MkValue val) = return $ MkLocalPath val

instance IsValue Title where
    fromValue (MkValue val) = return $ MkTitle val

instance IsValue TargetPath where
    fromValue (MkValue val) = return $ MkTargetPath val

instance IsValue Text where
    fromValue (MkValue val) = return $ MkText val

instance IsValue CSSID where
    fromValue (MkValue val) = return $ MkID val

instance IsValue Author where
    fromValue (MkValue val) = return $ MkAuthor val

instance IsValue MenuInformation where
    fromValue (MkValue "true") = return $ MkMenu True
    fromValue (MkValue "false") = return $ MkMenu False
    fromValue (MkValue _) = fail "MenuInformation needs to be either true or false"

class (Monad m) => External m where
    readResource :: LocalPath -> m String

instance External IO where
    readResource (MkLocalPath pth) = readFile pth

data FileSource = MkFileSource LocalPath String deriving (Show)

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
  | Picture URLPath Title CSSID
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

data PageInformation = MkPageInformation Title TargetPath (Maybe MenuInformation) deriving (Show)

data Page = MkPage PageInformation LightBlock deriving (Show)

-- Tokenized character sets for paragraphs
wordLetter = alphaNum <|> char '.' <|> char ':' <|> char '!'
    <|> char '?' <|> char ',' <|> char ')' <|> char '('

charToken a = tokenize $ char a

stringToken str = tokenize $ string str

braceCommand str p = between
      (try $ charToken '{' *> stringToken str)
      (char '}')
      p

ensureStartOfLine :: Monad m => ParsecT s u m ()
ensureStartOfLine = do
  pos <- getPosition
  guard (sourceColumn pos == 1)

preventStartOfLine :: Monad m => ParsecT s u m ()
preventStartOfLine = do
  pos <- getPosition
  guard (sourceColumn pos /= 1)

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
  MkPageInformation <$> getArgument "title" opts
    <*> getArgument "path" opts
    <*> getOptionalArgument "menu" opts

parseLink :: Parser LightAtom
parseLink = braceCommand "link" $ do
    opts <- parseArgumentsWithDefaults ["path", "text"]
    Link <$> (getArgument "path" opts)
         <*> (getArgument "text" opts)


parseBook :: Parser LightAtom
parseBook = braceCommand "book" $ do
  opts <- parseArgumentsWithDefaults ["title", "author"]
  case Book
    <$> (getArgument "title" opts)
    <*> (getArgument "author" opts)
    <*> (getOptionalArgument "link" opts) of
    Nothing -> fail "Arguments for author not present"
    Just bk -> return bk

parsePicture :: Parser LightBlock
parsePicture = ensureStartOfLine *> (tokenizeBlock $ braceCommand "picture" $ do
  opts <- parseArgumentsWithDefaults ["path", "title", "id"]
  Picture <$> (getArgument "path" opts)
    <*> (getArgument "title" opts)
    <*> (getArgument "id" opts))

parsePublicationList :: Parser LightBlock
parsePublicationList = ensureStartOfLine *> (tokenizeBlock $ braceCommand "publications" $ do
  opts <- parseArgumentsWithDefaults ["path"]
  PublicationList <$> (getArgument "path" opts))

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

parseAtom :: Parser LightAtom
parseAtom = (parseLinebreak <|> parseWord <|> parseLink <|> parseBook)

parseParagraph :: Parser LightBlock
parseParagraph = tokenizeBlock $ do
  text <- optionalSepBy parseSpace parseAtom
  return $ Para $ removeRedundantSpaces text

parseDirect :: Parser LightBlock
parseDirect = tokenizeBlock $ do
  text <- optionalSepBy parseSpace parseAtom
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
parseBlock = parseEnumeration
    <|> parseHeader <|> parseParagraph
    <|> parseComment <|> parsePicture <|> parsePublicationList

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

interpretMarkLight :: External m => Page -> m U.Html
interpretMarkLight (MkPage pageinfo lightblock) = do
    blocks <- renderLightBlock lightblock
    return $ generatePageHeader pageinfo blocks

generatePageHeader :: PageInformation -> U.Html -> U.Html
generatePageHeader pageinfo bdy = U.page (renderTitle pageinfo) $
    renderMenu pageinfo <> U.pageTitle (renderTitle pageinfo) <> bdy

renderMenu :: PageInformation -> U.Html
renderMenu (MkPageInformation _ _ Nothing) = mempty
renderMenu (MkPageInformation _ _ (Just (MkMenu False))) = mempty
renderMenu (MkPageInformation _ _ (Just (MkMenu True))) = U.menuBlock

renderTitle :: PageInformation -> U.Html
renderTitle (MkPageInformation (MkTitle title) _ _) = U.toHtml title

renderLightBlock :: External m => LightBlock -> m U.Html
renderLightBlock (Header las) = return $ U.headline (renderLightAtomList las)
renderLightBlock (Plain lbs) = mconcat <$> traverse renderLightBlock lbs
renderLightBlock (Para las) = return $ U.p $ (renderLightAtomList las)
renderLightBlock (Direct las) = return $ renderLightAtomList las
renderLightBlock (Enumeration las) = do
    blocks <- traverse renderLightBlock las
    return $ U.ul $ mconcat $ U.li <$> blocks
renderLightBlock (Picture (MkURLPath path) (MkTitle title) (MkID id)) = return $ U.image path title id
renderLightBlock Comment = return $ mempty
renderLightBlock (PublicationList path) = do
    bib <- readResource path
    return $ BG.generateBibliography bib

renderLightAtomList :: [LightAtom] -> U.Html
renderLightAtomList las = mconcat $ renderLightAtom <$> las

renderLightAtom :: LightAtom -> U.Html
renderLightAtom (Word str) = U.toHtml str
renderLightAtom (Link (MkURLPath path) (MkText txt)) = U.link path txt
renderLightAtom Space = U.toHtml (" " :: String)
renderLightAtom Newline = U.br
renderLightAtom (Book (MkTitle title) (MkAuthor author) Nothing) = (U.em $ U.toHtml $ title) <> (U.toHtml $ " by " ++ author)
renderLightAtom (Book (MkTitle title) (MkAuthor author) (Just (MkURLPath path))) = (U.em $ U.link path title) <> (U.toHtml $ " by " ++ author)
