{-# LANGUAGE OverloadedStrings #-}

module TeachingPage (
teachingPage
) where

import Utils

mac18Instanton = do
    headline "McMaster / Instantons and 4-manifolds"
    p $ "We will go through the book of Freed and Uhlenbeck, and learn about Instantons and 4-manifolds and the involved analysis. This will culminate in a proof of Donaldson's diagonalisation theorem."
    p $ "H312, Fridays 11:15"

macMCZ = do
    headline "McMaster / Math 1ZC3"
    rightPicture desc "./images/matrix.jpg" "A magic matrix" "matrix"
      where desc = do
              p $ "Introduction to linear algebra for engineers. We cover linear independence, dimension, determinants, eigenvalues, diagonalisation, and complex numbers."
              p $ link "https://www.childsmath.ca/childsa/forms/main_login.php" "Course page"

macAlgTop = do
    headline "McMaster / Math 731"
    p "Introduction to algebraic topology with focus on the fundamental group\
    \ and homology."
    p $ link "./algtop.html" "Course page"

bulbDescription = do
  p $ do
    "After recalling why light bulbs are standard in dimension 3, we go through\
    \ Gabai's proof ["
    link "https://arxiv.org/abs/1705.09989" "ArXiv:1705.09989"
    "] of the corresponding statement in dimension 4.\
    \ Along the way, we pick up the necessary background on embedded Morse theory and\
    \ diffeomorphism groups."
  p $ "H312, Fridays 13:00"

bulbSeminar = do
    headline "McMaster / Light bulbs in dimension 3 and 4"
    rightPicture bulbDescription
      "./images/bulb.jpg" "A light bulb" "bulb"

swSeminarDescription = do
  p "We study the approach to Seiberg-Witten theory of Bauer-Furuta using stable homotopy theory."
  p $ link "http://www.patrickorson.com/SWstable/" "Seminar page"

swSeminar = do
    headline "UQAM / SW stable homotopy theory"
    rightPicture swSeminarDescription "./images/vectorfield.jpg" "Flow of a vectorfield" "vectorfield"

mcLinAlg = do
    headline "McGill / Math 123"
    p "Introduction to linear algebra with applications from probability theory and optimisation."
    p $ do
        "Exercises on "
        link "http://msr02.math.mcgill.ca/webwork2" "Webworks"

mcCplx = do
    headline "McGill / Math 381"
    p "Complex calculus focusing on methods for evaluating integrals over domains \
    \in the complex plane and the computation of various transforms and their application \
    \in engineering."
    p $ do
        link "./pdf/Math381/Homework1.pdf" "Homework1"
        " "
        link "./pdf/Math381/Homework2.pdf" "Homework2"
        " "
        link "./pdf/Math381/Homework3.pdf" "Homework3"
        " "
        link "./pdf/Math381/Homework4.pdf" "Homework4"
        " "
        link "./pdf/Math381/Homework5.pdf" "Homework5"

teachingPage :: Html
teachingPage = page "Teaching" $ do
        menuBlock
        mac18Instanton
        macMCZ
        bulbSeminar
        macAlgTop
        swSeminar
        mcLinAlg
        mcCplx
