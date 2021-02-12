{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad.Fix
import Text.Blaze.Html.Renderer.String
import GHC.IO.Encoding
import Data.String

import qualified HtmlInterface as HI
import HtmlInterface(HasMenu(..))
import Effects.ConstructEnvironment
import Effects.RunWithEnvironment

import MarkLightParser
import MarkLight.Types
import Types

import Data.Time.Clock
import System.FilePath
import System.Directory

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


websiteFromPaths :: (ReadLocal m, MonadFail m, HasMenu m, WriteLocal m) => [LocalPath] -> m ()
websiteFromPaths fs  = mapM runMarkLight fs >> return ()

markLightFolder = "resources/marklight/"

main :: IO ()
main = do
    fs <- listDirectory markLightFolder
    let files = MkLocalPath <$> (markLightFolder </>) <$> fs
    env <- computeEnvironment emptyEnvironment $ websiteFromPaths $ files
    execWithEnvironment env $ websiteFromPaths $ files
