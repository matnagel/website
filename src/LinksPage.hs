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
        link "https://ms.mcmaster.ca/~nagelm1/topologyseminar.html" "McMaster Topology & Geometry"
        br
        link "https://seminars.math.toronto.edu/seminars/list/events.py/process?start" "University of Toronto"
        br
        link "http://www.fields.utoronto.ca/calendar/month/" "Fields Institute"
