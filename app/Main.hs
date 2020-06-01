{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Blaze.Html.Renderer.String
import GHC.IO.Encoding
import MyContent

import MarkLightParser

import Data.Time.Clock

loadMarkLight :: String -> IO Page
loadMarkLight path = do
    cont <- readFile path
    parseMarkLight (MkLocalPath path) cont

runMarkLight :: String -> String -> IO ()
runMarkLight input output = do
    page <- loadMarkLight input
    html <- interpretMarkLight $ page
    writeFile output $ renderHtml html


main :: IO ()
main = do
    setLocaleEncoding utf8
    time <- getCurrentTime
    writeFile "output/index.html" $ renderHtml $ indexPage
    writeFile "output/teaching.html" $ renderHtml $ teachingPage
    -- writeFile "output/publications.html" $ renderHtml $ publicationPage bib
    runMarkLight "resources/marklight/publications.mu" "output/publications.html"
    writeFile "output/algtop.html" $ renderHtml $ algTopPage
    writeFile "output/topologyseminar.html" $ renderHtml $ topSemPage (utctDay time)
    -- writeFile "output/misc.html" $ renderHtml $ miscPage
    -- writeFile "output/geotop.html" $ renderHtml $ geoTopPage
    runMarkLight "resources/marklight/misc.mu" "output/misc.html"
    runMarkLight "resources/marklight/geotop.mu" "output/geotop.html"
