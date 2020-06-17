{-# LANGUAGE OverloadedStrings #-}

module MarkLight.Arguments
  ( parseArguments,
    parseArgumentsWithDefaults,
    Arguments,
    getArgument,
    getOptionalArgument,
    getArgumentWithDefault,
    quote,
    Value(..),
    IsValue(..),
    TotalKeys(..),
    PositionalKeys(..)
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
import Text.Parsec.Combinator (between, sepBy)
-- import Text.Parsec.Prim
import Text.Parsec.String
import Prelude hiding (div, head, id)

import MarkLight.Types

newtype Arguments = MkArguments (M.Map String Value)

newtype TotalKeys = MkTotalKeys [String]

newtype PositionalKeys = MkPositionalKeys [String]

argumentsGetter :: Arguments -> M.Map String Value
argumentsGetter (MkArguments arg) = arg

argumentsSetter :: Arguments -> M.Map String Value -> Arguments
argumentsSetter (MkArguments arg) sarg = MkArguments $ sarg

argumentsOptic = lens argumentsGetter argumentsSetter

data AccumulateArguments = MkAcc [Value] Arguments

instance Semigroup Arguments where
  (<>) (MkArguments a) (MkArguments b) = MkArguments (b <> a)

instance Monoid Arguments where
  mempty = MkArguments mempty

instance Semigroup AccumulateArguments where
  (<>) (MkAcc as arg) (MkAcc bs brg) = MkAcc (as <> bs) (arg <> brg)

instance Monoid AccumulateArguments where
  mempty = MkAcc [] mempty

tokenize :: Parser a -> Parser a
tokenize p = p <* spaces

charToken a = tokenize $ char a

keyletters = tokenize $ many1 alphaNum

valueLetters = tokenize $ many1 (alphaNum <|> char ':' <|> char '/' <|> char ' '
    <|> char '.' <|> char ',' <|> char '-' <|> char '~')

showArgument (k, v) = k ++ "=" ++ show v

interleave sep [] = []
interleave sep (x : []) = x
interleave sep (x : ys) = x <> sep <> interleave sep ys

instance Show Arguments where
  show (MkArguments a) = "(" ++ interleave ", " (showArgument <$> (M.toList a)) ++ ")"

quote p = tokenize $ between (char '\"') (charToken '\"') p

parseArgument :: TotalKeys -> Parser Arguments
parseArgument (MkTotalKeys allowedKeys) = tokenize $ do
  key <- keyletters
  if notElem key allowedKeys
    then fail $ "Unknown key: " ++ key
    else (parseSetValue key) <|> (return $ setKeyPresent key)

setKeyPresent :: String -> Arguments
setKeyPresent key = MkArguments $ M.singleton key (MkBool True)

parseSetValue :: String -> Parser Arguments
parseSetValue key = do
  charToken '='
  value <- MkValue <$> quote valueLetters
  return $ MkArguments $ M.singleton key value

parseUnassignedArgument :: Parser AccumulateArguments
parseUnassignedArgument = tokenize $ do
    value <- MkValue <$> quote valueLetters
    return $ MkAcc [value] mempty

parseAccumulateArguments :: TotalKeys -> Parser AccumulateArguments
parseAccumulateArguments allowedKeys = tokenize $ do
    opts <- sepBy (parseUnassignedArgument
                    <|> (MkAcc [] <$> parseArgument allowedKeys)) $ optional (charToken ',')
    return $ mconcat opts

isArgumentPresent :: MonadFail m => Arguments -> String -> m ()
isArgumentPresent (MkArguments arg) key = case M.member key arg of
    True -> return $ ()
    False -> fail $ "Argument error: Key " ++ key ++ "=\"..\" is missing."

parseArgumentsWithDefaults :: TotalKeys -> PositionalKeys -> Parser Arguments
parseArgumentsWithDefaults allowedKeys posKeys@(MkPositionalKeys posList) = tokenize $ do
    args <- addKeysWithDefaults posKeys <$> parseAccumulateArguments allowedKeys
    foldr (*>) (return args) (isArgumentPresent args <$> posList)

addKeysWithDefaults :: PositionalKeys -> AccumulateArguments -> Arguments
addKeysWithDefaults (MkPositionalKeys []) (MkAcc _ args) = args
addKeysWithDefaults _ (MkAcc [] args) = args
addKeysWithDefaults (MkPositionalKeys (k:ks)) aa@(MkAcc (v:vs) args) = if (M.notMember k $ view argumentsOptic args)
    then addKeysWithDefaults (MkPositionalKeys ks) (MkAcc vs $ over argumentsOptic (M.insert k v) args)
    else addKeysWithDefaults (MkPositionalKeys ks) aa

parseArguments :: TotalKeys -> Parser Arguments
parseArguments alloweds = parseArgumentsWithDefaults alloweds (MkPositionalKeys [])

getArgument :: (IsValue a, MonadFail m) => String -> Arguments -> m a
getArgument str (MkArguments mp) = fromMaybe
    (fail $ "No key present for \"" ++ str ++ "\"")
    (fromValue <$> M.lookup str mp)

getOptionalArgument :: (IsValue a, MonadFail m) => String -> Arguments -> m (Maybe a)
getOptionalArgument str (MkArguments mp) = sequence $ fromValue <$> M.lookup str mp

getArgumentWithDefault :: (IsValue a, MonadFail m) => String -> a -> Arguments -> m a
getArgumentWithDefault str def (MkArguments mp) = fromMaybe (return $ def) (fromValue <$> M.lookup str mp)
