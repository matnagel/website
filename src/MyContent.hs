{-# LANGUAGE OverloadedStrings #-}

module MyContent (
indexPage,
teachingPage,
publicationPage,
algTopPage,
linksPage,
topSemPage
) where

import BibliographyGenerator
import AlgTopPage (algTopPage)
import TopologySeminar (topSemPage)
import LinksPage (linksPage)
import TeachingPage
import Utils

publicationPage bib = page "Publications" $ do
        menuBlock
        pageTitle "Publications"
        generateBibliography bib

indexPage :: Html
indexPage = page "Index" $ do
        menuBlock
        indexContent

indexContent :: Html
indexContent = do
        pageTitle "Matthias Nagel"
        aboutmeBlock
        researchBlock

researchBlock = do
            headline "Interests"
            p "My field of study is geometric topology. I am interested in concordance of links, and surfaces of minimal genus in 3- and 4-manifolds."
            p "One of my favourite tools for studying these questions is the signature and twisted Reidemeister torsion."

oxaddress = pre "nagel (at) maths.ox.ac.uk\n\n\
        \Mathematical Institute\n\
        \University of Oxford\n\
        \Oxford, United Kingdom"

aboutmeBlock :: Html
aboutmeBlock = rightPicture (oxaddress) "./images/myself2.jpg" "Photo of myself" "myphoto"
