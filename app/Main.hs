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
import Effects.ConstructEnvironment
import Effects.RunWithEnvironment

import MarkLightParser
import MarkLight.Types

import Data.Time.Clock
import System.FilePath


outputFolder = "output/" :: FilePath

loadMarkLight :: (ReadLocal m, MonadFail m) => LocalPath -> m Page
loadMarkLight path = do
    cont <- readResource path
    parseMarkLight path cont

runMarkLight :: (ReadLocal m, MonadFail m, HasMenu m, WriteLocal m) => LocalPath -> m ()
runMarkLight input = do
    page <- loadMarkLight input
    html <- interpretMarkLight page
    let MkTargetPath output = (getPagePath . getPageMetadata) page
    writeResource (MkLocalPath (outputFolder </> output)) $ renderHtml html

website :: (ReadLocal m, MonadFail m, HasMenu m, WriteLocal m) => m ()
website  = do
   -- lift $ setLocaleEncoding utf8
   -- time <- lift $ getCurrentTime
   -- lift $ writeFile "output/index.html" $ renderHtml $ indexPage
   -- lift $ writeFile "output/teaching.html" $ renderHtml $ teachingPage
    -- writeFile "output/publications.html" $ renderHtml $ publicationPage bib
    runMarkLight "resources/marklight/publications.mu"
    runMarkLight "resources/marklight/index.mu"
   -- lift $ writeFile "output/algtop.html" $ renderHtml $ algTopPage
   -- lift $ writeFile "output/topologyseminar.html" $ renderHtml $ topSemPage (utctDay time)
    -- writeFile "output/misc.html" $ renderHtml $ miscPage
    -- writeFile "output/geotop.html" $ renderHtml $ geoTopPage
    runMarkLight "resources/marklight/misc.mu"
    runMarkLight "resources/marklight/geotop.mu"

main :: IO ()
main = do
    env <- (computeEnvironment emptyEnvironment $ website)
    execWithEnvironment env website
