{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module MarkLight.Arguments
  (
    quote,
    Argument (..),
    parseArg,
    parseQuotedString,
    parseBool,
    Parseable (..),
  )
where

import Types

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

import Text.ParserCombinators.Parsec.Number
import Text.Parsec.Char
import Text.Parsec.Combinator (between, sepBy, sepBy1)
import Text.Parsec.String
import Prelude hiding (div, head, id)

import MarkLight.Types
import Data.Typeable
import Data.Foldable

data Value = forall a . Typeable a => MkValue a

type ValueStorage = M.Map String Value

data Argument a where
    FromKey :: Typeable a => String -> Parser a -> Argument a
    FromKeyDefault :: Typeable a => String -> Parser a -> a -> Argument a
    FromFlag :: String -> (Bool -> a) -> Argument a
    Application :: Argument (a->b) -> Argument a -> Argument b
    Lift :: a -> Argument a


instance (Functor Argument) where
    fmap f aa = Application (Lift f) aa

instance (Applicative Argument) where
    pure = Lift
    (<*>) = Application


keyValueParser :: Typeable a => String -> Parser a -> Parser (String, Value)
keyValueParser key p = do
    tokenize $ try (string key)
    charToken '='
    val <- tokenize $ p
    return $ (key, MkValue val)

flagParser :: String -> Parser (String, Value)
flagParser key = do
    tokenize $ try (string key)
    return $ (key, MkValue True)

extractParserList :: Argument a -> [Parser (String, Value)]
extractParserList (Lift _) = []
extractParserList (FromKey key p) = [keyValueParser key p]
extractParserList (FromFlag key _) = [flagParser key]
extractParserList (FromKeyDefault key p _) = [keyValueParser key p]
extractParserList (Application f a) = extractParserList f ++ extractParserList a

noDuplicateInsert :: MonadFail m => [(String, Value)] -> ValueStorage -> m ValueStorage
noDuplicateInsert [] storage = return $ storage
noDuplicateInsert ((key, val):xs) storage =
    if (M.notMember key storage)
    then noDuplicateInsert xs $ M.insert key val storage
    else fail $ " The key \"" ++ key ++ "\" occured twice"

parseValueStorage :: [Parser (String, Value)] -> Parser ValueStorage
parseValueStorage ps = do
    kvs <- sepBy (asum ps) $ optional (charToken ',')
    noDuplicateInsert kvs mempty

computeArg :: MonadFail m => ValueStorage -> Argument a -> m a
computeArg _ (Lift a) = return $ a
computeArg opt (FromKey key _) = case M.lookup key opt of
        Nothing -> fail $ "Required Key " ++ key ++ " has not been set"
        Just (MkValue val) -> case (cast val) of
                Nothing -> fail "Type error: this should never happen"
                Just a -> return a
computeArg opt (FromFlag key f) = case M.lookup key opt of
        Nothing -> return $ f False
        Just (MkValue val) -> case (cast val) of
                Nothing -> fail "Type error: this should never happen"
                Just a -> return $ f a
computeArg opt (FromKeyDefault key _ def) = case M.lookup key opt of
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
    opt <- parseValueStorage (extractParserList aa)
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

parseBool :: Parser Bool
parseBool = (string "true" >> return True) <|> (string "false" >> return False)

instance Parseable FlagRegisterMenuEntry where
    stdParser = MkRegisterMenuEntryFlag <$> parseBool

instance Parseable FlagIncludesMenu where
    stdParser = MkIncMenuFlag <$> parseBool

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
        <|> (string "right" >> return StyleRight)

instance Parseable PictureSize where
    stdParser = (string "height:" >> MkSizeHeight <$> floating)
        <|> (string "width:" >> MkSizeWidth <$> floating)
