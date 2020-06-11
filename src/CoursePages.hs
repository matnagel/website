{-# LANGUAGE OverloadedStrings #-}

module CoursePages (
algTopPage,
geoTopPage
) where

import Prelude hiding (head, div, id)
import HtmlInterface

geoTopPage = page "Introduction to 3-manifolds" (
    h1 "Introduction to 3-manifolds"
    <> image (storageETH "images/braid.jpg") "A braid" "braid"
    <> desItem "Announcements" ( ul $ do
        li $ "Since all classroom teaching has been suspended, the course will continue online. I will provide you with recorded lectures, which are complemented by zoom meetings."
        )
    <> desItem "Homework" (ul $ do
        li $ link (storageETH "pdf/20geotop/homework1.pdf") "Homework 1"
        li $ link (storageETH "pdf/20geotop/homework2.pdf") "Homework 2"
        li $ link (storageETH "pdf/20geotop/homework3.pdf") "Homework 3"
        )
    <> desItem "Textbooks" ( ul $ do
        li $ book "Knots and links" "by D. Rolfsen"
        li $ book "3-manifolds" "by J. Hempel"
        li $ book "Differential Topology" "by T. Bröcker, K. Jänich"
        li $ book "Differential Topology" "by C. Wall"
        li $ book (link "http://people.math.harvard.edu/~kupers/teaching/272x/index.html" "Diffeomorphism groups of disks") "by A. Kupers"
        )
    <> desItem "Contact" ( do
        "Matthias Nagel"
        br
        "matthias.nagel (at) math.ethz.ch"
        br
        "HG G28"
        )
    )

algTopPage = page "M731 Algebraic Topology" $ do
  h1 "M731 Algebraic Topology"
  algTopContent

desItem :: Html -> Html -> Html
desItem desc cont = do
    h2 desc
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
    li $ book "Topology" "by Munkres"
    li $ book "Basic Concepts of Algebraic Topology" "by F. Croom"
    li $ do
          link "http://www.math.uchicago.edu/~may/CONCISE/ConciseRevised.pdf" "A Concise Course in Algebraic Topology"
          "by P. May"

book title author = (em title) >> " " >> author
