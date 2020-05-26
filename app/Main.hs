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

main :: IO ()
main = do
    setLocaleEncoding utf8
    bib <- readFile "resources/json/publications.json"
    time <- getCurrentTime
    writeFile "output/index.html" $ renderHtml $ indexPage
    writeFile "output/teaching.html" $ renderHtml $ teachingPage
    writeFile "output/publications.html" $ renderHtml $ publicationPage bib
    writeFile "output/algtop.html" $ renderHtml $ algTopPage
    writeFile "output/topologyseminar.html" $ renderHtml $ topSemPage (utctDay time)
    -- writeFile "output/misc.html" $ renderHtml $ miscPage
    -- writeFile "output/geotop.html" $ renderHtml $ geoTopPage
    miscPage <- loadMarkLight "resources/marklight/misc.mu"
    writeFile "output/misc.html" $ renderHtml $ interpretMarkLight $ miscPage
    geoTopPage <- loadMarkLight "resources/marklight/geotop.mu"
    writeFile "output/geotop.html" $ renderHtml $ interpretMarkLight $ geoTopPage
