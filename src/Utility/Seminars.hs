{-# LANGUAGE OverloadedStrings #-}

module Utility.Seminars (
  Talk (..),
  sortTalks,
  University,
  Name,
  Title,
  Abstract,
  Time,
  renderTalk,
  (<|),
  speaker,
  name,
  title,
  abstract,
  day,
  dayInfo,
  before
) where


import Prelude hiding (head, div, id, span)
import Text.Blaze.Html5 ( (!), Html )
import Text.Blaze.Html5 hiding (main, title, time)
import Text.Blaze.Html5.Attributes hiding (title, span, name)

import Data.Monoid

import Utility.Separators

import Data.List
import Data.Time.Calendar
import Data.Time.Format

type University = Html
type Name = Html
type Title = Html
type Abstract = Html
type Information = Html

data Time = Time Day (Maybe Information)

data Talk = Talk Time Speaker (Maybe Title) (Maybe Abstract)
  | Announcement Time Html


compareTimeOfTalks :: Talk -> Talk -> Ordering
compareTimeOfTalks (Talk (Time lt _) _ _ _) (Talk (Time rt _) _ _ _) = compare lt rt

sortTalks :: [Talk] -> [Talk]
sortTalks = sortBy compareTimeOfTalks

before :: Day -> Talk -> Bool
before ct (Talk (Time dt _) _ _ _) = dt < ct

data Speaker = Speaker Name (Maybe University)

(<|) :: (a -> b) -> a -> b
(<|) f a = f a

infixl 1 <|

speaker nme uni = Speaker nme $ Just uni
name nme = Speaker nme Nothing
title = Just
abstract = Just
day y m d = Time (fromGregorian y m d) Nothing

dayInfo :: Integer -> Int -> Int -> Html -> Time
dayInfo y m d info = Time (fromGregorian y m d) (Just info)

renderTime :: Time -> Html
renderTime (Time day Nothing) = toHtml $ formatTime defaultTimeLocale "%B %e" day
renderTime (Time day (Just desc)) = ( toHtml $ formatTime defaultTimeLocale "%B %e" day ) <> ", " <> desc

renderUniversity :: University -> Html
renderUniversity university = "(" <> university <> ")"

renderSpeaker :: Speaker -> Html
renderSpeaker (Speaker name university) = flatten [
  separatedEntry name " ",
  separatedEntryMaybe (renderUniversity <$> university) mempty]

renderAbstract = p ! class_ "talks"
renderTitle = div ! class_ "talktitle"

renderTalk :: Talk -> Html
renderTalk (Talk time speaker title abstract) = h3 ! class_ "talks" $ do
  flatten [
    separatedEntry (renderTime time) " - ",
    separatedEntry (renderSpeaker speaker) ":",
    separatedEntryMaybe (renderTitle <$> title) mempty,
    separatedEntryMaybe (renderAbstract <$> abstract) mempty
    ]
