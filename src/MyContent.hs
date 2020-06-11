{-# LANGUAGE OverloadedStrings #-}

module MyContent (
indexPage,
teachingPage,
publicationPage,
algTopPage,
geoTopPage,
miscPage,
topSemPage
) where

import Prelude hiding (div)
import BibliographyGenerator
import CoursePages (algTopPage, geoTopPage)
import TopologySeminar (topSemPage)
import MiscPage (miscPage)
import TeachingPage
import HtmlInterface

publicationPage bib = page "Publications"
    $ menuBlock
    <> pageTitle "Publications"
    <> generateBibliography bib

indexPage :: Html
indexPage = page "Index"
        $ menuBlock
        <> indexContent

indexContent :: Html
indexContent = pageTitle "Matthias Nagel"
        <> aboutmeBlock
        <> researchBlock

researchBlock = headline "Interests"
            <> p "My field of study is geometric topology. I am interested in geometric objects up to dimension 4 and their embeddings. In particular, I study knots and their mathematical properties. Here is an example of an interesting (part of a) knot:"
            <> (flex ! style "justify-content:center; margin:2ex" $ image (storageETH "images/clasp.jpg") "Clasp" "clasp")
            <> p "I am especially curious about concordance, a special equivalence relation on knots. This relation is intimately tied to questions on the topology of 4-manifolds and their smooth structures."
            <> projects

projects = p
    $ "If you are interested in writing bachelor thesis or a semester project with me, here are some possible "
    <> link (storageETH "projects.pdf") "topics"
    <> "."

ethaddress = pre "matthias.nagel (at) math.ethz.ch\n\n\
        \Department of Mathematics\n\
        \ETH Zurich, Switzerland"

whoIam = p
        $ "I am a postdoc on the job market. Here is a " 
        <> link (storageETH "nagelCV.pdf") "CV"
        <> "."

aboutmeContent = flex ! style "flex-direction:column" 
        $ ethaddress 
        <> whoIam

aboutmeBlock :: Html
aboutmeBlock = rightPicture aboutmeContent (storageETH "images/myself3.jpg") "Photo of myself" "myphoto"
