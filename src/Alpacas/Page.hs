{-# LANGUAGE OverloadedStrings #-}
module Alpacas.Page where

import Data.String              ( fromString )
import Snap.Types
import Text.Blaze.Html5         ( (!) )
import Text.Blaze.Renderer.Utf8 ( renderHtml )
import qualified Data.Text as T
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

data Page = Page { pageTitle   :: H.Html
                 , pageHead    :: H.Html
                 , pageContent :: H.Html
                 }

noHtml :: H.Html
noHtml = return ()

-- | Construct an empty HTML page given a title
page :: H.Html -> Page
page t = Page { pageTitle = t
              , pageHead = noHtml
              , pageContent = noHtml
              }

type Renderer = Page -> H.Html

type AlterPage = Page -> Page

data Script = ScriptInline T.Text
            | ScriptSrc T.Text

data Stylesheet = StylesheetInline T.Text
                | StylesheetLink T.Text

scriptToHtml :: Script -> H.Html
scriptToHtml t =
    case t of
      ScriptInline s -> f $ H.text s
      ScriptSrc s    ->
          let src = A.src $ fromString $ T.unpack s
          in f ! src $ noHtml
    where
      f = H.script ! A.type_ "text/javascript"

cssToHtml :: Stylesheet -> H.Html
cssToHtml t =
    case t of
      StylesheetInline s -> H.style ! A.type_ "text/css" $ H.text s
      StylesheetLink s   ->
          let href = A.href $ fromString $ T.unpack s
          in H.link ! A.type_ "text/css" ! A.rel "stylesheet" ! href

modifyHead :: (H.Html -> H.Html) -> AlterPage
modifyHead f p = p { pageHead = f $ pageHead p }

addScript :: Script -> AlterPage
addScript s = modifyHead (>> scriptToHtml s)

addCss :: Stylesheet -> AlterPage
addCss s = modifyHead (>> cssToHtml s)

modifyBody :: (H.Html -> H.Html) -> AlterPage
modifyBody f p = p { pageContent = f $ pageContent p }

prependBody :: H.Html -> AlterPage
prependBody h = modifyBody (h >>)

appendBody :: H.Html -> AlterPage
appendBody h = modifyBody (>> h)

renderPage :: Page -> H.Html
renderPage p =
    H.docTypeHtml $ do
      H.head $ do
        H.title $ pageTitle p
        pageHead p
      H.body $ do
        H.h1 $ pageTitle p
        pageContent p

respondPage :: Renderer -> Page -> Snap ()
respondPage r p = do
  modifyResponse $ setContentType "text/html"
  writeLBS $ renderHtml $ r p
