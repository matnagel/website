{-# LANGUAGE OverloadedStrings #-}

module Utility.Seminars (
  Talk (..),
  University,
  Name,
  Title,
  Abstract,
  Date,
  renderTalk,
  (<|),
  speaker,
  name,
  title,
  abstract,
  day,
  before
) where


import Prelude hiding (head, div, id, span)
import Text.Blaze.Html5 ( (!), Html )
import Text.Blaze.Html5 hiding (main, title, time)
import Text.Blaze.Html5.Attributes hiding (title, span, name)

import Data.Monoid

import Utility.Separators

import Data.Time.Calendar
import Data.Time.Format

type University = Html
type Name = Html
type Title = Html
type Abstract = Html
type Date = Day

data Talk = Talk Date Speaker (Maybe Title) (Maybe Abstract)
  | Announcement Date Html

before :: Day -> Talk -> Bool
before ct (Talk dt _ _ _) = dt < ct

data Speaker = Speaker Name (Maybe University)

(<|) :: (a -> b) -> a -> b
(<|) f a = f a

infixl 1 <|

speaker nme uni = Speaker nme $ Just uni
name nme = Speaker nme Nothing
title = Just
abstract = Just
day = fromGregorian

renderDay :: Day -> Html
renderDay dt = toHtml $ formatTime defaultTimeLocale "%B %e" dt

renderUniversity :: University -> Html
renderUniversity university = "(" <> university <> ")"

renderSpeaker :: Speaker -> Html
renderSpeaker (Speaker name university) = flatten [
  separatedEntry name " ",
  separatedEntryMaybe university mempty]

renderAbstract = p ! class_ "talks"
renderTitle = div ! class_ "talktitle"

renderTalk :: Talk -> Html
renderTalk (Talk day speaker title abstract) = h3 ! class_ "talks" $ do
  flatten [
    separatedEntry (renderDay day) " - ",
    separatedEntry (renderSpeaker speaker) ":",
    separatedEntryMaybe (renderTitle <$> title) mempty,
    separatedEntryMaybe (renderAbstract <$> abstract) mempty
    ]
