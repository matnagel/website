{-# LANGUAGE OverloadedStrings #-}

module MyContent (
indexPage,
teachingPage,
publicationPage,
algTopPage,
miscPage,
topSemPage
) where

import Prelude hiding (div)
import BibliographyGenerator
import AlgTopPage (algTopPage)
import TopologySeminar (topSemPage)
import MiscPage (miscPage)
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
            p "My field of study is geometric topology. I am interested in geometric objects up to dimension 4 and their embeddings. In particular, I study knots such as the one below:"
            flex ! style "justify-content:center; margin-bottom:1ex" $ image "images/clasp.jpg" "Clasp" "clasp"
            -- p "My research focuses on concordance of links, and surfaces of minimal genus in 3- and 4-manifolds and their relation to smooth structures."

ethaddress = pre "matthias.nagel (at) math.ethz.ch\n\n\
        \Department of Mathematics\n\
        \ETH Zurich, Switzerland"

whoIam = p $ do 
        "I am a postdoc on the job market. Here is a " 
        link (storageETH "nagelCV.pdf") "CV"
        "."

aboutmeContent = flex ! style "flex-direction:column" $ do
        ethaddress 
        whoIam

aboutmeBlock :: Html
aboutmeBlock = do 
        rightPicture aboutmeContent (storageETH "images/myself2.jpg") "Photo of myself" "myphoto"
