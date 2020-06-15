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
    IsValue(..)
  )
where

import Control.Monad
import Optics
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
import Prelude hiding (div, head, id)

import MarkLight.Types

newtype Arguments = MkArguments (M.Map String Value)

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

parseArgument :: [String] -> Parser Arguments
parseArgument allowedkey = tokenize $ do
  key <- keyletters
  if notElem key allowedkey
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

parseAccumulateArguments :: [String] -> Parser AccumulateArguments
parseAccumulateArguments allowedkeys = tokenize $ do
    opts <- sepBy (parseUnassignedArgument
                    <|> (MkAcc [] <$> parseArgument allowedkeys)) $ optional (charToken ',')
    return $ mconcat opts

isArgumentPresent :: MonadFail m => Arguments -> String -> m ()
isArgumentPresent (MkArguments arg) key = case M.member key arg of
    True -> return $ ()
    False -> fail $ "Argument error: Key " ++ key ++ "=\"..\" is missing."

parseArgumentsWithDefaults :: [String] -> [String] -> Parser Arguments
parseArgumentsWithDefaults allowedkeys defaultkeys = tokenize $ do
    args <- addKeysWithDefaults defaultkeys <$> parseAccumulateArguments allowedkeys
    foldr (*>) (return args) (isArgumentPresent args <$> defaultkeys)

addKeysWithDefaults :: [String] -> AccumulateArguments -> Arguments
addKeysWithDefaults [] (MkAcc _ args) = args
addKeysWithDefaults _ (MkAcc [] args) = args
addKeysWithDefaults (k:ks) aa@(MkAcc (v:vs) args) = if (M.notMember k $ view argumentsOptic args)
    then addKeysWithDefaults ks (MkAcc vs $ over argumentsOptic (M.insert k v) args)
    else addKeysWithDefaults ks aa

parseArguments :: [String] -> Parser Arguments
parseArguments alloweds = parseArgumentsWithDefaults alloweds []

getArgument :: (IsValue a, MonadFail m) => String -> Arguments -> m a
getArgument str (MkArguments mp) = case M.lookup str mp of
    Nothing -> fail $ "No value for key \"" ++ str ++ "\""
    Just val -> fromValue val

getOptionalArgument :: (IsValue a, MonadFail m) => String -> Arguments -> m (Maybe a)
getOptionalArgument str (MkArguments mp) = case M.lookup str mp of
    Nothing -> return $ Nothing
    Just val -> Just <$> fromValue val

getArgumentWithDefault :: (IsValue a, MonadFail m) => String -> a -> Arguments -> m a
getArgumentWithDefault str def (MkArguments mp) = case M.lookup str mp of
    Nothing -> return $ def
    Just val -> fromValue val
