{-# LANGUAGE OverloadedStrings #-}

module AlgTopPage (
algTopPage
) where

import Prelude hiding (head, div, id)
import Text.Blaze.Html5 ( (!), Html, h1, h2, br, li, ul, em, p )
import Text.Blaze.Html5.Attributes ( class_ )
import Utils

algTopPage = page "M731 Algebraic Topology" $ do
  h1 "M731 Algebraic Topology"
  algTopContent

desItem :: Html -> Html -> Html
desItem desc cont = do
    h2 ! class_ "many" $ desc
    cont

algTopContent = do
  image (storageETH "images/covering.jpg") "A cover of the circle" "covering"
  desItem "Final exam" $ finalexam
  desItem "Homework" $ homeworkList
  desItem "Instructor" instructorItem
  desItem "Course outline" $ do
    link (storageETH "pdf/math731/Outline.pdf") "Course outline"
  desItem "Lecture" $ do
    "Tuesdays and Fridays, 9:30 - 11:00"
    br
    "HH410"
  desItem "Topics"
    "Topological spaces, CW complexes, fundamental groups,\
    \ covering spaces, and homology"
  desItem "Textbook" booksItem

homeworkList = ul $ do
    li $ homework (storageETH "pdf/math731/Homework6.pdf") "Homework 6" "December 5"
    li $ homework (storageETH "pdf/math731/Homework5.pdf") "Homework 5" "November 28"
    li $ homework (storageETH "pdf/math731/Homework4.pdf") "Homework 4" "November 17"
    li $ homework (storageETH "pdf/math731/Homework3.pdf") "Homework 3" "October 27"
    li $ homework (storageETH "pdf/math731/Homework2.pdf") "Homework 2" "October 6"
    li $ homework (storageETH "pdf/math731/Homework1.pdf") "Homework 1" "September 22"

instructorItem = do
    "Matthias Nagel"
    br
    "matthias.nagel (at) math.mcmaster.ca"
    br
    "H414, Wednesdays 9:30"

finalexam = do
  p $ "The final exam is a take-home exam. It will be distributed Wednesday, December 6 - 10:00 at my office M414 and is due on Friday, December 8 - 10:00. It has to be returned to me before the deadline passes at M414."
  p $ link (storageETH "pdf/math731/Finalexam.pdf") "Final exam"

booksItem = ul $ do
    li $ do
        link "https://www.math.cornell.edu/~hatcher/AT/ATpage.html" "Algebraic Topology"
        " by A. Hatcher"
    li $ book "Topology" " by Munkres"
    li $ book "Basic Concepts of Algebraic Topology" " by F. Croom"
    li $ do
          link "http://www.math.uchicago.edu/~may/CONCISE/ConciseRevised.pdf" "A Concise Course in Algebraic Topology"
          " by P. May"

book title author = (em title) >> author
