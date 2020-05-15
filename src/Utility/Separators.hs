module Utility.Separators (
  separatedEntry,
  separatedEntryMaybe,
  flatten
) where

import Data.Monoid
import Data.Foldable

data SeparatedEntry a = Entry a a | EmptyEntry

instance Semigroup a => Semigroup (SeparatedEntry a) where
  (<>) (Entry x s) (Entry y t) = Entry (x <> s <> y) t
  (<>) EmptyEntry q = q
  (<>) p EmptyEntry = p

instance Semigroup a => Monoid (SeparatedEntry a) where
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
