{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}


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
CoreInline (..),
compileHtml,
cssClass
) where

import Prelude hiding (head, div)
import Text.Blaze.Html5 ( (!), Html )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Types

import Data.String
import Data.Maybe
import Data.Monoid

import Optics.TH
import Optics

data CoreInlineElement = Newline
    | Space
    | Text String
    | Em [CoreInlineElement]
    | Link URLPath Text

type CoreInline = [CoreInlineElement]

compileInlineElement :: CoreInlineElement -> Html
compileInlineElement Newline = H.br
compileInlineElement Space = H.toHtml (" " :: String)
compileInlineElement (Text str) = H.toHtml str
compileInlineElement (Link (MkURLPath url) (MkText text)) = H.a
    ! A.href (fromString url) $ (fromString text)
compileInlineElement (Em ins) =  H.em $ mconcat $ compileInlineElement <$> ins

data CSS = MkCSS { _cssClass :: Maybe String }

makeLenses ''CSS

emptyCSS = MkCSS Nothing

type CSSTransformation = CSS -> CSS

data CoreHtml = Monoid [CoreHtml]
    | Paragraph CoreInline
    | Direct CoreInline
    | Header Text
    | Pre String
    | Enumeration [CoreInline]
    | Div CSSTransformation CoreHtml
    | Picture URLPath Text PictureSize
    | HFlex [CoreHtml]
    | Lift Html

compileInline :: CoreInline -> Html
compileInline cin = mconcat $ compileInlineElement <$> cin

compileCSS :: CSSTransformation -> H.Attribute
compileCSS f = mconcat $ catMaybes [A.class_ . fromString <$> (view cssClass css)]
    where css = f emptyCSS

compileHtml :: CoreHtml -> Html
compileHtml (Monoid hls) = mconcat $ compileHtml <$> hls
compileHtml (Direct hls) = compileInline hls
compileHtml (Paragraph para) = H.p $ mconcat $ compileInlineElement <$> para
compileHtml (Header (MkText text)) = H.p $ headline $ toHtml text
compileHtml (Pre text) = H.pre $ toHtml text
compileHtml (Div trans hl) = (H.div H.! compileCSS trans) $ compileHtml hl
compileHtml (Picture (MkURLPath path) (MkText text) size) = image path text size
compileHtml (HFlex hls) = flex $ mconcat $ compileHtml <$> encapsulateMonoid <$> hls
compileHtml (Lift hl) = hl
compileHtml (Enumeration inline) = H.li $ mconcat $ H.ul . compileInline <$> inline

encapsulateMonoid :: CoreHtml -> CoreHtml
encapsulateMonoid (Monoid ls) = Div id (Monoid ls)
encapsulateMonoid x = x

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
