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

import MarkLightParser

import Data.Time.Clock

data Capsule = MkCapsule { getMenuEntries :: [HI.MenuEntry] }

data ReverseStateM m s a = MkReverseStateM (s -> m (s, a))

instance Functor m => Functor (ReverseStateM m s) where
    fmap g (MkReverseStateM f) = MkReverseStateM (\s -> (fmap g) <$> (f s))

instance (Monad m) => Applicative (ReverseStateM m s) where
    pure a = MkReverseStateM (\s -> return $ (s,a))
    (<*>) (MkReverseStateM sf) (MkReverseStateM sa) = MkReverseStateM (\s -> do
            (future, a) <- sa s
            (past, f) <- sf future
            return (past, f a)
        )

instance (Monad m, MonadFix m) => Monad (ReverseStateM m s)  where
    (>>=) (MkReverseStateM sx) rsf = MkReverseStateM (\s -> mdo
            let (MkReverseStateM rg) = rsf a
            (future, b) <- rg s
            (past, a) <- sx future
            return (past, b)
        )

type ComputeM = ReverseStateM IO Capsule

lift :: (Monad m) => m a -> ReverseStateM m s a
lift ma = MkReverseStateM (\s -> do
    a <- ma
    return (s,a))

setState :: (Monad m) => m s -> ReverseStateM m s ()
setState ms = MkReverseStateM $ const $ do
    s <- ms
    return (s,())

getState :: (Monad m) => ReverseStateM m s s
getState = MkReverseStateM $ (\s -> do
    return (s,s))

class WriteLocal m where
    writeResource :: LocalPath -> String -> m ()

instance ReadLocal IO where
    readResource (MkLocalPath pth) = readFile pth

instance HasMenu IO where
    getMenu = return $ HI.menuBlock
    registerMenu _ = return $ ()

instance WriteLocal IO where
    writeResource (MkLocalPath path) = writeFile path

instance IsString LocalPath where
    fromString str = MkLocalPath str

instance WriteLocal ComputeM where
    writeResource (MkLocalPath path) cont = lift $ writeFile path cont

instance HasMenu ComputeM where
    getMenu = return $ HI.menuBlock
    registerMenu _ = undefined

instance ReadLocal ComputeM where
    readResource (MkLocalPath pth) = MkReverseStateM (\s -> do
        file <- readFile pth
        return (s,file))

loadMarkLight :: (ReadLocal m, MonadFail m) => LocalPath -> m Page
loadMarkLight path = do
    cont <- readResource path
    parseMarkLight path cont

runMarkLight :: (ReadLocal m, MonadFail m, HasMenu m, WriteLocal m) => LocalPath -> LocalPath -> m ()
runMarkLight input output = do
    page <- loadMarkLight input
    html <- interpretMarkLight $ page
    writeResource output $ renderHtml html


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
