{-# LANGUAGE OverloadedStrings #-}

module MarkLight.Arguments
  ( parseArguments,
    parseArgumentsWithDefaults,
    Arguments,
    getArgument,
    getOptionalArgument,
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
import qualified Utils as U
import Prelude hiding (div, head, id)


newtype Value = MkValue String

instance Show Value where
    show (MkValue str) = show str

class IsValue a where
    fromValue :: MonadFail m => Value -> m a

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

parseArgument :: Parser Arguments
parseArgument = tokenize $ do
  key <- keyletters
  charToken '='
  value <- MkValue <$> quote valueLetters
  return $ MkArguments $ M.singleton key value

parseUnassignedArgument :: Parser AccumulateArguments
parseUnassignedArgument = tokenize $ do
    value <- MkValue <$> quote valueLetters
    return $ MkAcc [value] mempty

parseAccumulateArguments :: Parser AccumulateArguments
parseAccumulateArguments = tokenize $ do
    opts <- sepBy (parseUnassignedArgument
                    <|> (MkAcc [] <$> parseArgument)) $ optional (charToken ',')
    return $ mconcat opts

isArgumentPresent :: MonadFail m => Arguments -> String -> m ()
isArgumentPresent (MkArguments arg) key = case M.member key arg of
    True -> return $ ()
    False -> fail $ "Argument error: Key " ++ key ++ "=\"..\" is missing."

parseArgumentsWithDefaults :: [String] -> Parser Arguments
parseArgumentsWithDefaults keys = tokenize $ do
    args <- addKeysWithDefaults keys <$> parseAccumulateArguments
    foldr (*>) (return args) (isArgumentPresent args <$> keys)

addKeysWithDefaults :: [String] -> AccumulateArguments -> Arguments
addKeysWithDefaults [] (MkAcc _ args) = args
addKeysWithDefaults _ (MkAcc [] args) = args
addKeysWithDefaults (k:ks) aa@(MkAcc (v:vs) args) = if (M.notMember k $ view argumentsOptic args)
    then addKeysWithDefaults ks (MkAcc vs $ over argumentsOptic (M.insert k v) args)
    else addKeysWithDefaults ks aa

parseArguments :: Parser Arguments
parseArguments = parseArgumentsWithDefaults []

getArgument :: (IsValue a, MonadFail m) => String -> Arguments -> m a
getArgument str (MkArguments mp) = case M.lookup str mp of
    Nothing -> fail $ "No value for key \"" ++ str ++ "\""
    Just val -> fromValue val

getOptionalArgument :: (IsValue a, MonadFail m) => String -> Arguments -> m (Maybe a)
getOptionalArgument str (MkArguments mp) = case M.lookup str mp of
    Nothing -> return $ Nothing
    Just val -> Just <$> fromValue val
