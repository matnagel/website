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
            p "My field of study is geometric topology. I am interested in geometric objects up to dimension 4 and their embeddings. In particular, I study knots and their mathematical properties. Here is an example of an interesting (part of a) knot:"
            flex ! style "justify-content:center; margin:2ex" $ image (storageETH "images/clasp.jpg") "Clasp" "clasp"
            p "I am especially curious about concordance, a special equivalence relation on knots. This relation is intimately tied to questions on the topology of 4-manifolds and their smooth structures."
            -- p "The study of knots and their concordance classes is intimately related to the topology of 4-manifolds and their smooth structures. This is also the focus of my work on surfaces of minimal genus in 3- and 4-manifolds."  

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
