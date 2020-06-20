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
    WriteLocal(..)
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
import Prelude hiding (div, head, id)

import MarkLight.Arguments
import MarkLight.Types

-- Tokenized character sets for paragraphs
wordLetter = alphaNum <|> char '.' <|> char ':' <|> char '!'
    <|> char '?' <|> char ',' <|> char ')' <|> char '(' <|> char '-'

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


pageInformationArg :: Argument PageInformation
pageInformationArg =
    FromKey "registerMenu" (stdParser) $
    FromKey "addMenu" (stdParser) $
    FromKey "path" (stdParser) $
    FromKey "title" (stdParser) $ Lift MkPageInformation


parsePageInformation :: Parser PageInformation
parsePageInformation = tokenizeBlock $ braceCommand "page" $ parseArg pageInformationArg

-- do
--  opts <- parseArgumentsWithDefaults (MkTotalKeys ["path", "title", "addMenu", "registerMenu"]) (MkPositionalKeys ["path","title"])
--  MkPageInformation <$> getArgument "title" opts
--    <*> getArgument "path" opts
--    <*> getArgumentWithDefault "addMenu" (MkIncMenuFlag False) opts
--    <*> getArgumentWithDefault "registerMenu" (MkRegisterMenuEntryFlag False) opts

linkArg :: Argument LightAtom
linkArg = FromKey "text" (stdParser :: Parser Text) $
          FromKey "path" (stdParser :: Parser URLPath) $
          Lift (Link)

parseLink :: Parser LightAtom
parseLink = braceCommand "link" $ parseArg linkArg

-- do
--     opts <- parseArgumentsWithDefaults (MkTotalKeys ["path", "title"]) (MkPositionalKeys ["path", "text"])
--     Link <$> (getArgument "path" opts)
--          <*> (getArgument "text" opts)

bookArg :: Argument LightAtom
bookArg = FromKeyDefault "title" stdParser Nothing $
    FromKey "author" stdParser $
    FromKey "link" stdParser $ Lift Book

parseBook :: Parser LightAtom
parseBook = braceCommand "book" $ parseArg bookArg

-- do
--   opts <- parseArgumentsWithDefaults (MkTotalKeys ["path", "title", "link"])
--     (MkPositionalKeys ["title", "author"])
--   case Book
--     <$> (getArgument "title" opts)
--     <*> (getArgument "author" opts)
--     <*> (getOptionalArgument "link" opts) of
--     Nothing -> fail "Arguments for author not present"
--     Just bk -> return bk
--

pictureArg :: Argument LightBlock
pictureArg = FromKey "style" (stdParser) $ 
    FromKey "id" (MkID <$> parseQuotedString) $
    FromKey "title" (MkTitle <$> parseQuotedString) $ 
    FromKey "path" (MkURLPath <$> parseQuotedString) $
    (Lift Picture)

parsePicture :: Parser LightBlock
parsePicture = ensureStartOfLine *> (tokenizeBlock $
    braceCommand "picture" $ parseArg pictureArg)


-- $ do
--   opts <- parseArgumentsWithDefaults
--     (MkTotalKeys ["path", "title", "id", "style"])
--     (MkPositionalKeys ["path", "title", "id"])
--   Picture <$> (getArgument "path" opts)
--     <*> (getArgument "title" opts)
--     <*> (getArgument "id" opts)
--     <*> (getArgumentWithDefault "style" NoStyle opts))

publicationListArg :: Argument LightBlock
publicationListArg = FromKey "publication" stdParser $ Lift PublicationList

parsePublicationList :: Parser LightBlock
parsePublicationList = ensureStartOfLine *> (tokenizeBlock $ braceCommand "publications"
    $ parseArg publicationListArg)

-- do
--   opts <- parseArgumentsWithDefaults (MkTotalKeys ["path"]) (MkPositionalKeys ["path"])
--   PublicationList <$> (getArgument "path" opts))

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

interpretMarkLight :: (HasMenu m, ReadLocal m) => Page -> m HI.Html
interpretMarkLight (MkPage pageinfo lightblock) = do
    blocks <- renderLightBlock lightblock
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

renderLightBlock :: ReadLocal m => LightBlock -> m HI.Html
renderLightBlock (Header las) = return $ HI.headline (renderLightAtomList las)
renderLightBlock (Plain lbs) = mconcat <$> traverse renderLightBlock lbs
renderLightBlock (Para las) = return $ HI.p $ (renderLightAtomList las)
renderLightBlock (Direct las) = return $ renderLightAtomList las
renderLightBlock (Enumeration las) = do
    blocks <- traverse renderLightBlock las
    return $ HI.ul $ mconcat $ HI.li <$> blocks
renderLightBlock (Picture (MkURLPath path) (MkTitle title) (MkID id) NoStyle) = return $ HI.image path title id
renderLightBlock (Picture (MkURLPath path) (MkTitle title) (MkID id) StyleCentered) = return $ (HI.flex HI.! HI.style "justify-content:center; margin:2ex" $ HI.image path title id)
renderLightBlock Comment = return $ mempty
renderLightBlock (PublicationList path) = do
    bib <- readResource path
    return $ BG.generateBibliography bib

renderLightAtomList :: [LightAtom] -> HI.Html
renderLightAtomList las = mconcat $ renderLightAtom <$> las

renderLightAtom :: LightAtom -> HI.Html
renderLightAtom (Word str) = HI.toHtml str
renderLightAtom (Link (MkURLPath path) (MkText txt)) = HI.link path txt
renderLightAtom Space = HI.toHtml (" " :: String)
renderLightAtom Newline = HI.br
renderLightAtom (Book (MkTitle title) (MkAuthor author) Nothing) = (HI.em $ HI.toHtml $ title) <> (HI.toHtml $ " by " ++ author)
renderLightAtom (Book (MkTitle title) (MkAuthor author) (Just (MkURLPath path))) = (HI.em $ HI.link path title) <> (HI.toHtml $ " by " ++ author)
