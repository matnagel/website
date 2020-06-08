{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Utils (
addheader,
rightPicture,
flex,
image,
page,
link,
homework,
menuBlock,
mathjax,
headline,
pageTitle,
br,
Html,
p,
h1,
h2,
li,
em,
ul,
pre,
toHtml,
storageETH,
divClass,
div,
(!),
style,
(<>),
MenuEntry(..)
) where

import Prelude hiding (head, div, id)
import Text.Blaze.Html5 ( (!), Html )
--import Text.Blaze.Html5 hiding (link, title, style)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.String
import Data.Monoid

data MenuEntry = MkMenuEntry String String

style = A.style
pre = H.pre
em = H.em
p = H.p
br = H.br
toHtml = H.toHtml
h1 = H.h1
h2 = H.h2

li = H.li
ul = H.ul
div = H.div

page mtitle content = H.docTypeHtml $ do
  addheader mtitle
  content

divId :: H.AttributeValue -> Html -> Html
divId id content = H.div ! A.id id $ content

divClass :: H.AttributeValue -> Html -> Html
divClass cls content = H.div ! A.class_ cls $ content

headline = H.h2
pageTitle = H.h1

addheader mtitle = H.head $ do
        H.meta ! A.httpEquiv "content-type" ! A.content "text/html; charset=utf-8"
        H.title mtitle
        H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "./css/default.css"

flex = divClass "flex"

image url desc pid = H.img ! A.src (fromString url) ! A.alt (fromString desc) ! A.id (fromString pid)

link :: String -> String -> Html
link url name = H.a ! A.href (fromString url) $ (fromString name)

rightPicture left url desc pid = flex $ do
        left
        (image url desc pid) ! style "margin-left: 1ex"

homework url name due = do
  link url name
  ", due "
  due

mathjax :: Html
mathjax = do
  H.script ! A.type_ "text/x-mathjax-config" $
    H.preEscapedString "MathJax.Hub.Config({\
    \ tex2jax: {\
    \ inlineMath: [ ['$','$'] ],\
    \  processEscapes: true\
    \ }\
    \ });"
  H.script ! A.type_ "text/javascript"
    ! A.src "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2/MathJax.js?config=TeX-AMS_CHTML"
    $ mempty

menuBlock = do
        divId "menu" $ do
            divId "logo" $ do
                link "./index.html" $ "Home"
            divId "navigation" $ do
                link "./teaching.html" $ "Teaching"
                link "./publications.html" $ "Publications"
                link "./misc.html" $ "Misc"

storageETH x = fromString $ "https://people.math.ethz.ch/~managel/website/" ++ x
