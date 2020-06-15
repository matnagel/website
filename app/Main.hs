{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Applicative
import Control.Monad.Fix
import Text.Blaze.Html.Renderer.String
import GHC.IO.Encoding
import Data.String
import MyContent

import qualified HtmlInterface as HI
import HtmlInterface(HasMenu(..))
import Interpretation.ConstructEnvInt
import Interpretation.RunEnvInt

import MarkLightParser

import Data.Time.Clock

loadMarkLight :: (ReadLocal m, MonadFail m) => LocalPath -> m Page
loadMarkLight path = do
    cont <- readResource path
    parseMarkLight path cont

runMarkLight :: (ReadLocal m, MonadFail m, HasMenu m, WriteLocal m) => LocalPath -> LocalPath -> m ()
runMarkLight input output = do
    page <- loadMarkLight input
    html <- interpretMarkLight $ page
    writeResource output $ renderHtml html

website :: (ReadLocal m, MonadFail m, HasMenu m, WriteLocal m) => m ()
website  = do
   -- lift $ setLocaleEncoding utf8
   -- time <- lift $ getCurrentTime
   -- lift $ writeFile "output/index.html" $ renderHtml $ indexPage
   -- lift $ writeFile "output/teaching.html" $ renderHtml $ teachingPage
    -- writeFile "output/publications.html" $ renderHtml $ publicationPage bib
    runMarkLight "resources/marklight/publications.mu" "output/publications.html"
   -- lift $ writeFile "output/algtop.html" $ renderHtml $ algTopPage
   -- lift $ writeFile "output/topologyseminar.html" $ renderHtml $ topSemPage (utctDay time)
    -- writeFile "output/misc.html" $ renderHtml $ miscPage
    -- writeFile "output/geotop.html" $ renderHtml $ geoTopPage
    runMarkLight "resources/marklight/misc.mu" "output/misc.html"
    runMarkLight "resources/marklight/geotop.mu" "output/geotop.html"

main :: IO ()
main = do
    env <- (computeEnvironment emptyEnvironment $ website)
    execWithEnvironment env website
