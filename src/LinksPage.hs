{-# LANGUAGE OverloadedStrings #-}

module LinksPage (
linksPage
) where

import Utils

linksPage :: Html
linksPage = page "Links" $ do
        menuBlock
        pageTitle "Links"
        headline "Seminars"
        link "https://www.maths.ox.ac.uk/events/list/all" "Seminars at Oxford"
