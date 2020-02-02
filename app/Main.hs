{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Blaze.Html.Renderer.String
import GHC.IO.Encoding
import MyContent

import Data.Time.Clock

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
    writeFile "output/misc.html" $ renderHtml $ miscPage
    writeFile "output/geotop.html" $ renderHtml $ geoTopPage
