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
    ReadLocal(..),
    WriteLocal(..),
    parseHeader
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
    try
  )
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String
import qualified HtmlInterface as HI
import HtmlInterface (HasMenu(..))
import qualified BibliographyGenerator as BG
import Prelude hiding (div, head)

import Data.Foldable
import Data.Typeable

import MarkLight.Arguments
import Types
import MarkLight.Types

notAtLineBegin :: Parser ()
notAtLineBegin = do
    col <- sourceColumn <$> getPosition
    guard (col > 1)

-- Tokenized character sets for paragraphs
wordLetter = alphaNum <|> char '.' <|> char ':' <|> char '!'
    <|> char '?' <|> char ',' <|> char ')' <|> char '(' <|> (notAtLineBegin >> char '-')



blank = char ' '
nonNewlineSpace = blank <|> tab
spacesWithAtMostOneNewline = do
    many nonNewlineSpace
    optional (newline >> (many nonNewlineSpace))

tokenize :: Parser a -> Parser a
tokenize p = p <* spacesWithAtMostOneNewline

charToken a = tokenize $ char a

stringToken str = tokenize $ try $ string str

braceCommand str p = between
      (try $ charToken '{' *> stringToken str)
      (char '}')
      p

parseCommand :: Typeable a => String -> Argument a -> Parser a
parseCommand str arg = braceCommand str $ parseArg arg

parseCommands :: Typeable a => [(String, Argument a)] -> Parser a
parseCommands xs = asum $ (\(str,arg) -> parseCommand str arg) <$> xs

parseSpace :: Parser LightAtom
parseSpace = tokenize ( (lookAhead space) >> return Space)

parseWord :: Parser LightAtom
parseWord = Word <$> (many1 wordLetter)

parseLinebreak :: Parser LightAtom
parseLinebreak = tokenize $ try (stringToken "<br>" >> (return Newline))

tokenizeBlock :: Parser a -> Parser a
tokenizeBlock p = p <* spaces

pageInformationArg :: Argument PageInformation
pageInformationArg = MkPageInformation
    <$>| FromKey "title" (stdParser)
    <*>| FromKey "path" (stdParser)
    <*>| FromFlag "addMenu" (MkIncMenuFlag)
    <*>| FromFlag "registerMenu" (MkRegisterMenuEntryFlag)

parsePageInformation :: Parser PageInformation
parsePageInformation = parseCommand "page" pageInformationArg

linkArg :: Argument LightAtom
linkArg = Link
    <$>| FromKey "path" (stdParser :: Parser URLPath)
    <*>| FromKey "text" (stdParser :: Parser Text)

parseLink :: Parser LightAtom
parseLink = parseCommand "link" linkArg

bookArg :: Argument LightAtom
bookArg = Book
    <$>| FromKey "title" stdParser
    <*>| FromKey "author" stdParser
    <*>| FromKeyDefault "link" (return <$> stdParser) Nothing

parseBook :: Parser LightAtom
parseBook = parseCommand "book" bookArg

braced p = between (charToken '{') (charToken '}') p


hflexArg :: Argument LightBlock
hflexArg = HFlex <$>| (FromKey "content" $ parser)
    where parser = do
            hbs <- between (charToken '[') (charToken ']')
                $ sepBy1 (braced $ mconcat <$> many1 parseBlock)
                $ charToken ','
            return hbs

parseHFlex :: Parser LightBlock
parseHFlex = parseCommand "hflex" hflexArg

parseHeader :: Parser LightBlock
parseHeader = tokenizeBlock $ do
  tokenize $ many1 (char '=')
  hdr <- many1 (wordLetter <|> blank)
  return $ Header $ MkText hdr

removeRedundantSpaces :: [LightAtom] -> [LightAtom]
removeRedundantSpaces [] = []
removeRedundantSpaces (Space:xs) = removeRedundantSpaces xs
removeRedundantSpaces (x:Space:Space:xs) = removeRedundantSpaces (x:Space:xs)
removeRedundantSpaces (x:Space:y:xs) = x:Space:removeRedundantSpaces (y:xs)
removeRedundantSpaces (x:xs) = x:removeRedundantSpaces xs

parseAtom :: Parser LightAtom
parseAtom = (parseLinebreak <|> parseWord <|> parseLink <|> parseBook)

optionalSepBy :: Parser a -> Parser a -> Parser [a]
optionalSepBy sep p = do
    as <- many1 (helper sep p <|> return <$> p)
    return $ concat as
    where helper sep p = try $ do
            ss <- sep
            ps <- p
            return (ss:ps:[])

parseInline :: Parser [LightAtom]
parseInline = removeRedundantSpaces <$> optionalSepBy parseSpace parseAtom

parseParagraph :: Parser LightBlock
parseParagraph = Para <$> parseInline

parseDirect :: Parser LightBlock
parseDirect = Direct <$> parseInline

parseEnumItem :: Parser [LightAtom]
parseEnumItem = tokenize $ do
    charToken '-'
    block <- parseInline
    return block

parseEnumeration :: Parser LightBlock
parseEnumeration = do
    blocks <- many1 parseEnumItem
    return $ Enumeration blocks

parseComment :: Parser LightBlock
parseComment = do
    charToken '#'
    manyTill anyChar (newline) >> return Comment

parsePreformated :: Parser LightBlock
parsePreformated = braceCommand "pre" $ Preformated <$> manyTill anyChar (lookAhead $ char '}')

publicationListArg :: Argument LightBlock
publicationListArg = PublicationList <$>| FromKey "src" stdParser

pictureArg :: Argument LightBlock
pictureArg = Picture
    <$>| FromKey "path" (MkURLPath <$> parseQuotedString)
    <*>| FromKey "title" (MkTitle <$> parseQuotedString)
    <*>| FromKey "size" stdParser
    <*>| FromKey "style" (stdParser)

parseBlock :: Parser LightBlock
parseBlock = tokenizeBlock $ parseEnumeration
    <|> parseHeader <|> parseParagraph <|> parseComment <|> parsePreformated
    <|> parseHFlex
    <|> parseCommands [("publications", publicationListArg),
        ("picture", pictureArg)]

parsePage :: Parser Page
parsePage = do
  pageInformation <- tokenizeBlock $ parsePageInformation
  elms <- many parseBlock
  eof
  return $ MkPage pageInformation $ mconcat elms

parseMarkLight :: MonadFail m => LocalPath -> String -> m Page
parseMarkLight (MkLocalPath path) cont = case parse parsePage path cont of
    Left err -> fail $ show err
    Right page -> return page

interpretMarkLight :: (HasMenu m, ReadLocal m) => Page -> m HI.Html
interpretMarkLight (MkPage pageinfo lightblock) = do
    blocks <- HI.compileHtml <$> translateLightBlock lightblock
    checkAndRegisterMenuEntry pageinfo
    generatePageHeader pageinfo blocks

checkAndRegisterMenuEntry :: (HasMenu m) => PageInformation -> m ()
checkAndRegisterMenuEntry pageinfo = case getFlagAddEntry pageinfo of
        (MkRegisterMenuEntryFlag False) -> return ()
        (MkRegisterMenuEntryFlag True) -> registerMenu $
            HI.MkMenuEntry (getPagePath pageinfo) (getPageTitle pageinfo)

generatePageHeader :: HasMenu m => PageInformation -> HI.Html -> m HI.Html
generatePageHeader pageinfo bdy = do
    menu <- renderMenu pageinfo
    return $ HI.page (renderPageTitle pageinfo) $ menu
        <> HI.pageTitle (renderPageTitle pageinfo) <> bdy

renderMenu :: HasMenu m => PageInformation -> m HI.Html
renderMenu pi = case getFlagIncludesMenu pi of
    (MkIncMenuFlag False) -> return $ mempty
    (MkIncMenuFlag True) -> getMenu

renderPageTitle :: PageInformation -> HI.Html
renderPageTitle pi = case getPageTitle pi of
    MkTitle title -> HI.toHtml title

translateLightBlock :: ReadLocal m => LightBlock -> m HI.CoreHtml
translateLightBlock (Header las) = return $ HI.Header $ las
translateLightBlock (Plain lbs) = mconcat <$> traverse translateLightBlock lbs
translateLightBlock (Para las) = return $ HI.Paragraph $ las >>= translateLightAtom
translateLightBlock (Direct las) = return $ HI.Direct $ las >>= translateLightAtom
translateLightBlock (Enumeration las) = return $ HI.Enumeration
    $ (\inline -> inline >>= translateLightAtom) <$> las
translateLightBlock (Picture path title size NoStyle) = return $ HI.Picture id path title size
translateLightBlock (Picture path title size StyleCentered) = return
    $ HI.HFlex (HI.setCSS "justify-content" "center" . HI.setCSS "margin" "2ex")
    $ [HI.Picture id path title size]
translateLightBlock (Picture path title size StyleRight) = return
    $ HI.Picture (HI.setCSS "float" "right") path title size
translateLightBlock Comment = return $ mempty
translateLightBlock (PublicationList path) = do
    bib <- readResource path
    return $ BG.generateBibliography bib
translateLightBlock (Preformated str) = return $ HI.Pre str
translateLightBlock (HFlex lbs) = HI.HFlex id <$> traverse translateLightBlock lbs

translateLightAtom :: LightAtom -> [HI.CoreInlineElement]
translateLightAtom (Word str) = return $ HI.Text str
translateLightAtom (Link path txt) = return $ HI.Link path txt
translateLightAtom Space = return $ HI.Space
translateLightAtom Newline = return $ HI.Newline
translateLightAtom (Book (MkTitle title) (MkAuthor author) Nothing) = [(HI.Em $ [HI.Text $ title]), (HI.Text $ " by " ++ author)]
translateLightAtom (Book (MkTitle title) (MkAuthor author) (Just path)) = [(HI.Em $ [HI.Link path (MkText title)]), (HI.Text $ " by " ++ author)]
