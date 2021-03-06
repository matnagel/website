{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}


module HtmlInterface (
page,
menuBlockFromList,
pageTitle,
Html,
H.toHtml,
MenuEntry(..),
HasMenu(..),
CoreInlineElement (..),
CoreHtml (..),
CoreInline,
ToCoreInlineElements (..),
compileHtml,
setStyle,
setClass
) where

import Prelude hiding (head, div)
import Text.Blaze.Html5 ( (!), Html )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Types

import Data.String
import Data.Maybe
import qualified Data.Map as M

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

type StyleContainer = M.Map String String
newtype CSSContainer = MkCSSContainer (Maybe String, StyleContainer)
type CSS = CSSContainer -> CSSContainer

instance Semigroup CSSContainer where
    (<>) (MkCSSContainer (a,b)) (MkCSSContainer (c,d)) = case a of
        Nothing -> MkCSSContainer (c, b <> d)
        x@(Just _) -> MkCSSContainer (x, b <> d)

data CoreHtml = Monoid [CoreHtml]
    | Paragraph CoreInline
    | Direct CoreInline
    | Header Text
    | Pre String
    | Enumeration [CoreInline]
    | Div CSS CoreHtml
    | Picture CSS URLPath Title PictureSize
    | HFlex CSS [CoreHtml]
    | Lift Html

class ToCoreInlineElements a where
    toCoreInlineElements :: a -> [CoreInlineElement]

compileInline :: CoreInline -> Html
compileInline cin = mconcat $ compileInlineElement <$> cin

buildStyle :: CSS -> H.Attribute
buildStyle f = A.style . fromString $ M.foldrWithKey (\k v str -> buildAttr k v <> str) mempty $ style
    where MkCSSContainer (_, style) = f $ MkCSSContainer (Nothing, mempty)
          buildAttr key v = key ++  ":" ++ v ++ ";"

buildClass :: CSS -> H.Attribute
buildClass f = case cls of
    Nothing -> mempty
    Just str -> A.class_ . fromString $ str
    where MkCSSContainer (cls, _) = f $ MkCSSContainer (Nothing, mempty)

compileCSS :: CSS -> H.Attribute
compileCSS f = buildClass f <> buildStyle f

compileHtml :: CoreHtml -> Html
compileHtml (Monoid hls) = mconcat $ compileHtml <$> hls
compileHtml (Direct hls) = compileInline hls
compileHtml (Paragraph para) = H.p $ mconcat $ compileInlineElement <$> para
compileHtml (Header (MkText text)) = H.p $ headline $ H.toHtml text
compileHtml (Pre text) = H.pre $ H.toHtml text
compileHtml (Div trans hl) = (H.div H.! compileCSS trans) $ compileHtml hl
compileHtml (Picture trans (MkURLPath path) (MkTitle text) size) = image trans path text size
compileHtml (HFlex css hls) = flex H.! compileCSS css $ mconcat $ compileHtml <$> encapsulateMonoid <$> hls
compileHtml (Enumeration inline) = H.ul $ mconcat $ (H.li . compileInline) <$> inline
compileHtml (Lift h) = h

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

renderMenuEntry :: MenuEntry -> Html
renderMenuEntry (MkMenuEntry (MkTargetPath url) (MkTitle name)) = link url name

class ToCSS a where
    toCSS :: a -> CSS

setStyle :: String -> String -> CSS
setStyle key str (MkCSSContainer (c, cont)) = MkCSSContainer (c, M.insert key str cont)

setClass :: String -> CSS
setClass key (MkCSSContainer (c, cont)) = MkCSSContainer (Just key, cont)


instance ToCSS PictureSize where
    toCSS (MkSizeHeight h) = setStyle "height" $ (show h) ++ "ex"
    toCSS (MkSizeWidth h) = setStyle "width" $ (show h) ++ "ex"

page :: Title -> CoreHtml -> Html
page mtitle content = H.docTypeHtml $ do
  addheader mtitle
  compileHtml content

divId :: H.AttributeValue -> Html -> Html
divId did content = H.div ! A.id did $ content

divClass :: H.AttributeValue -> Html -> Html
divClass cls content = H.div ! A.class_ cls $ content

headline :: Html -> Html
headline = H.h2

pageTitle :: Title -> Html
pageTitle (MkTitle title) = H.h1 $ fromString title

addheader :: Title -> Html
addheader (MkTitle mtitle) = H.head $ do
        H.meta ! A.httpEquiv "content-type" ! A.content "text/html; charset=utf-8"
        H.title $ fromString mtitle
        H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "./css/default.css"

flex :: Html -> Html
flex = divClass "flex"

image :: CSS -> String -> String -> PictureSize -> Html
image trans url desc size = H.img ! A.src (fromString url) ! A.alt (fromString desc) ! compileCSS (trans . toCSS size)

link :: String -> String -> Html
link url name = H.a ! A.href (fromString url) $ (fromString name)

-- mathjax :: Html
-- mathjax = do
--   H.script ! A.type_ "text/x-mathjax-config" $
--     H.preEscapedString "MathJax.Hub.Config({\
--     \ tex2jax: {\
--     \ inlineMath: [ ['$','$'] ],\
--     \  processEscapes: true\
--     \ }\
--     \ });"
--   H.script ! A.type_ "text/javascript"
--     ! A.src "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2/MathJax.js?config=TeX-AMS_CHTML"
--     $ mempty

menuBlockFromList :: [MenuEntry] -> Html
menuBlockFromList xs = divId "menu" $
    (divId "logo" $ link "./index.html" $ "Home")
    <> (divId "navigation" $ mconcat (renderMenuEntry <$> xs))
