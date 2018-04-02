{-# LANGUAGE OverloadedStrings #-}

module Utils (
addheader,
leftRight,
rightPicture,
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
em,
pre,
toHtml,
divClass
) where

import Prelude hiding (head, div, id)
import Text.Blaze.Html5 ( (!), Html )
import Text.Blaze.Html5 hiding (link, title)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Data.String

page mtitle content = docTypeHtml $ do
  addheader mtitle
  content

divId :: AttributeValue -> Html -> Html
divId id content = H.div ! A.id id $ content

divClass :: AttributeValue -> Html -> Html
divClass cls content = H.div ! A.class_ cls $ content

headline = H.h2
pageTitle = H.h1

addheader mtitle = head $ do
        meta ! httpEquiv "content-type" ! content "text/html; charset=utf-8"
        H.title mtitle
        H.link ! rel "stylesheet" ! type_ "text/css" ! href "./css/default.css"

leftRight left right = divClass "leftright" $ do
    H.div $ left
    H.div $ right

image url desc pid = img ! A.src url ! A.alt desc ! A.id pid

link :: String -> String -> Html
link url name = a ! href (fromString url) $ (fromString name)

rightPicture left url desc pid = leftRight left (image url desc pid)

homework url name due = do
  link url name
  ", due "
  due

mathjax :: Html
mathjax = do
  script ! type_ "text/x-mathjax-config" $
    preEscapedString "MathJax.Hub.Config({\
    \ tex2jax: {\
    \ inlineMath: [ ['$','$'] ],\
    \  processEscapes: true\
    \ }\
    \ });"
  script ! type_ "text/javascript"
    ! src "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2/MathJax.js?config=TeX-AMS_CHTML"
    $ mempty

menuBlock = do
        divId "menu" $ do
            divId "logo" $ do
                link "./index.html" $ "Home"
            divId "navigation" $ do
                link "./teaching.html" $ "Teaching"
                link "./publications.html" $ "Publications"
                link "./links.html" $ "Links"
