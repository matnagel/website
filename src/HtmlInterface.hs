{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module HtmlInterface (
addheader,
rightPicture,
flex,
image,
page,
link,
homework,
menuBlock,
menuBlockFromList,
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
MenuEntry(..),
HasMenu(..),
CoreInlineElement (..),
CoreHtml (..),
compileHtml
) where

import Prelude hiding (head, div, id)
import Text.Blaze.Html5 ( (!), Html )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- import qualified MarkLight.Types as ML
import Types

import Data.String
import Data.Monoid

data CoreInlineElement = Newline
    | Space
    | Em CoreInline
    | Text String
    | Link URLPath Text

type CoreInline = [CoreInlineElement]

compileInline :: CoreInlineElement -> Html
compileInline Newline = br
compileInline Space = toHtml (" " :: String)
compileInline (Text str) = toHtml str
compileInline (Link (MkURLPath url) (MkText text)) = H.a
    ! A.href (fromString url) $ (fromString text)
compileInline (Em inline) = H.em $ mconcat $ compileInline <$> inline

data CoreHtml = Monoid [CoreHtml]
    | Paragraph CoreInline
    | Direct CoreInline
    | Header Text
    | Pre String
    | Enumeration [CoreInline]
    | Div CoreHtml
    | Picture URLPath Text PictureSize
    | HFlex [CoreHtml]
    | Lift Html

compileHtml :: CoreHtml -> Html
compileHtml (Monoid hls) = mconcat $ compileHtml <$> hls
compileHtml (Paragraph para) = H.p $ mconcat $ compileInline <$> para
compileHtml (Header (MkText text)) = H.p $ headline $ toHtml text
compileHtml (Pre text) = H.pre $ toHtml text
compileHtml (Div hl) = H.div $ compileHtml hl
compileHtml (Picture (MkURLPath path) (MkText text) size) = image path text size
compileHtml (HFlex hls) = flex $ mconcat $ compileHtml <$> hls
compileHtml (Lift hl) = hl
compileHtml (Enumeration [inline]) = H.li $ mconcat $ H.ul . compileInline <$> inline

instance Semigroup CoreHtml where
  (<>) (Monoid a) (Monoid b) = Monoid (a <> b)
  (<>) a@(Monoid _) b = a <> Monoid [b]
  (<>) a b@(Monoid _) = Monoid [a] <> b
  (<>) a b = Monoid [a, b]

instance Monoid CoreHtml where
  mempty = Monoid []


class (Monad m) => HasMenu m where
  getMenu :: m Html
  registerMenu :: MenuEntry -> m ()

data MenuEntry = MkMenuEntry TargetPath Title

renderMenuEntry (MkMenuEntry (MkTargetPath url) (MkTitle name)) = link url name

class ToCSS a where
    toCSS :: a -> H.Attribute

instance ToCSS PictureSize where
    toCSS (MkSizeHeight h) = A.style $ fromString $ "height:" ++ (show h) ++ "ex"
    toCSS (MkSizeWidth h) = A.style $ fromString $ "width:" ++ (show h) ++ "ex"

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

image :: String -> String -> PictureSize -> Html
image url desc size = H.img ! A.src (fromString url) ! A.alt (fromString desc) ! toCSS size

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

menuBlock = menuBlockFromList [
    MkMenuEntry "./teaching.html" "Teaching",
    MkMenuEntry "./publications.html" "Publications",
    MkMenuEntry "./misc.html" "Misc"]

menuBlockFromList :: [MenuEntry] -> Html
menuBlockFromList xs = divId "menu" $
    (divId "logo" $ link "./index.html" $ "Home")
    <> (divId "navigation" $ mconcat (renderMenuEntry <$> xs))

storageETH x = fromString $ "https://people.math.ethz.ch/~managel/website/" ++ x
