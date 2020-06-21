{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module MarkLight.Arguments
  (
    quote,
    Argument (..),
    parseArg,
    parseQuotedString,
    Parseable (..),
    (<:)
  )
where

import Control.Monad
import Control.Applicative
import Data.Traversable
import Data.Maybe
import Optics
import qualified Data.Map as M
import qualified Text.Parsec as P
import Text.Parsec
  ( (<?>),
    char,
    eof,
    letter,
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
import Text.Parsec.Combinator (between, sepBy, sepBy1)
import Text.Parsec.String
import Prelude hiding (div, head, id)

import MarkLight.Types
import Data.Typeable
import Data.Foldable

data Value = forall a . Typeable a => MkValue a

data Argument a where
    FromKey :: Typeable a => String -> Parser a -> Argument a
    FromKeyDefault :: Typeable a => String -> Parser a -> Maybe a -> Argument (Maybe a)
    Application :: (Typeable a, Typeable b) => Argument (a->b) -> Argument a -> Argument b
    Lift :: Typeable a => a -> Argument a

(<:) :: (Typeable a, Typeable b) => Argument (a->b) -> Argument a -> Argument b
(<:) = Application

extractKeywordParserList :: Typeable a => Argument a -> [(String, Parser Value)]
extractKeywordParserList (Lift _) = []
extractKeywordParserList (FromKey key p) = return $ (key, (MkValue <$> p))
extractKeywordParserList (FromKeyDefault key p _) = return $ (key, (MkValue <$> p))
extractKeywordParserList (Application f a) = extractKeywordParserList f ++
    extractKeywordParserList a

constructKeywordParser :: (String, Parser Value) -> Parser (String, Value)
constructKeywordParser (key, p) = do
    tokenize $ try (string key)
    charToken '='
    val <- tokenize $ p
    return (key, val)

combineKeywordParser :: [Parser (String, Value)] -> Parser [(String, Value)]
combineKeywordParser ps = sepBy (asum ps) $ optional (charToken ',')

getFromList :: [(String, Value)] -> String -> Maybe Value
getFromList xs key = (\(k,v) -> v) <$> find (\(k, v) -> (k == key)) xs

computeArg :: MonadFail m => [(String, Value)] -> Argument a -> m a
computeArg opt (Lift a) = return $ a
computeArg opt (FromKey key _) = case getFromList opt key of
        Nothing -> fail $ "Required Key " ++ key ++ " has not been set"
        Just (MkValue val) -> case (cast val) of
                Nothing -> fail "Type error: this should never happen"
                Just a -> return $ a
computeArg opt (FromKeyDefault key _ def) = case getFromList opt key of
        Nothing -> return $ def
        Just (MkValue val) -> case (cast val) of
                Nothing -> fail "Type error: this should never happen"
                Just a -> return $ a
computeArg opt (Application af aa) = do
    f <- computeArg opt af
    a <- computeArg opt aa
    return $ f a


parseArg :: Typeable a => Argument a -> Parser a
parseArg aa  = tokenize $ do
    opt <- combineKeywordParser $ constructKeywordParser <$> extractKeywordParserList aa
    computeArg opt aa

tokenize :: Parser a -> Parser a
tokenize p = p <* spaces

charToken a = tokenize $ char a

valueLetters = (alphaNum <|> char ':' <|> char '/' <|> char ' '
    <|> char '.' <|> char ',' <|> char '-' <|> char '~')

quote p = tokenize $ between (char '\"') (charToken '\"') p

parseQuotedString :: Parser String
parseQuotedString = quote $ many valueLetters

class Parseable a where
    stdParser :: Parser a

boolParser :: Parser Bool
boolParser = (string "true" >> return True) <|> (string "false" >> return False)

instance Parseable FlagRegisterMenuEntry where
    stdParser = MkRegisterMenuEntryFlag <$> boolParser

instance Parseable FlagIncludesMenu where
    stdParser = MkIncMenuFlag <$> boolParser

instance Parseable TargetPath where
    stdParser = MkTargetPath <$> parseQuotedString

instance Parseable Text where
    stdParser = MkText <$> parseQuotedString

instance Parseable LocalPath where
    stdParser = MkLocalPath <$> parseQuotedString

instance Parseable Title where
    stdParser = MkTitle <$> parseQuotedString

instance Parseable Author where
    stdParser = MkAuthor <$> parseQuotedString

instance Parseable URLPath where
    stdParser = MkURLPath <$> parseQuotedString

instance Parseable Style where
    stdParser = (string "centered" >> (return $ StyleCentered))
        <|> (string "nostyle" >> return NoStyle)
