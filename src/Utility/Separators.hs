module Utility.Separators (
  separatedEntry,
  separatedEntryMaybe,
  flatten
) where

import Data.Monoid
import Data.Foldable

data SeparatedEntry a = Entry a a | EmptyEntry

instance Monoid a => Monoid (SeparatedEntry a) where
  mappend (Entry x s) (Entry y t) = Entry (x <> s <> y) t
  mappend EmptyEntry q = q
  mappend p EmptyEntry = p
  mempty = EmptyEntry

extract :: Monoid a => SeparatedEntry a -> a
extract EmptyEntry = mempty
extract (Entry x t) = x

flatten :: Monoid a => [SeparatedEntry a] -> a
flatten = extract . fold

separatedEntryMaybe :: Monoid a => Maybe a -> a -> SeparatedEntry a
separatedEntryMaybe Nothing _ = mempty
separatedEntryMaybe (Just content) seperator = Entry content seperator

separatedEntry :: a -> a -> SeparatedEntry a
separatedEntry = Entry
