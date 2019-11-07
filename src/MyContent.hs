{-# LANGUAGE OverloadedStrings #-}

module MyContent (
indexPage,
teachingPage,
publicationPage,
algTopPage,
miscPage,
topSemPage
) where

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
            p "My field of study is geometric topology. I am interested in concordance of links, and surfaces of minimal genus in 3- and 4-manifolds."
            p "One of my favourite tools for studying these questions is the signature and twisted Reidemeister torsion."

ethaddress = pre "matthias.nagel (at) math.ethz.ch\n\n\
        \Department of Mathematics\n\
        \ETH Zurich, Switzerland"

aboutField = do
		ethaddress
		p ( do 
			"I am on the job market. Here is a " 
			link (storageETH "nagelCV.pdf") "CV"
			"." )

aboutmeBlock :: Html
aboutmeBlock = rightPicture (aboutField) (storageETH "images/myself2.jpg") "Photo of myself" "myphoto"
