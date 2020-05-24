{-# LANGUAGE OverloadedStrings #-}

module MarkLight.Arguments
  ( parseArguments,
    Arguments,
    getArgument,
    quote
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

newtype Arguments = MkArguments (M.Map String String)

instance Semigroup Arguments where
  (<>) (MkArguments a) (MkArguments b) = MkArguments (b <> a)

instance Monoid Arguments where
  mempty = MkArguments mempty

tokenize :: Parser a -> Parser a
tokenize p = p <* spaces

charToken a = tokenize $ char a

keyletters = tokenize $ many1 alphaNum

valueLetters = tokenize $ many1 (alphaNum <|> char ':' <|> char '/' <|> char ' ' <|> char '.')

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
  value <- quote valueLetters
  return $ MkArguments $ M.singleton key value

parseArguments :: Parser Arguments
parseArguments = tokenize $ do
  opts <- sepBy parseArgument $ optional (charToken ',')
  return $ mconcat opts

getArgument :: String -> Arguments -> Maybe String
getArgument str (MkArguments mp) = M.lookup str mp
