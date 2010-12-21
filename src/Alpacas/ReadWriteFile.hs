{-# LANGUAGE OverloadedStrings #-}
module Alpacas.ReadWriteFile where

import Control.Applicative      ( (<|>) )
import Control.Monad.IO.Class   ( liftIO )
import Snap.Types
import System.IO.Error          ( isDoesNotExistError )
import Text.Blaze.Html5         ( (!) )
import Text.Blaze.Renderer.Utf8 ( renderHtml )
import System.FilePath          ( takeDirectory )
import System.Directory         ( createDirectoryIfMissing )
import qualified Data.Text.IO as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString as B

data Editable = Editable FilePath

dirListing :: Editable -> Snap ()
dirListing (Editable _root) = undefined

editFile :: FilePath -> Snap ()
editFile fn = method GET (respondPage =<< showEdit fn) <|>
              method POST (doUpdate fn)

-- XXX: redirect to show action instead of showing again
-- XXX: this requires the URL to the page
doUpdate :: FilePath -> Snap ()
doUpdate fn = do
  contentVal <- getParam "content"
  case contentVal of
    Nothing -> respondPage =<< showEdit fn
    Just v  ->
        do let d = takeDirectory fn
           liftIO $ createDirectoryIfMissing True d
           liftIO $ B.writeFile fn v
           respondPage =<< showEdit fn

data Page = Page { pageTitle   :: H.Html
                 , pageHead    :: H.Html
                 , pageContent :: H.Html
                 }

noHtml :: H.Html
noHtml = return ()

page :: H.Html -> Page
page t = Page { pageTitle = t
              , pageHead = noHtml
              , pageContent = noHtml
              }

respondPage :: Page -> Snap ()
respondPage p = do
  modifyResponse $ setContentType "text/html"
  writeLBS $ renderHtml $ H.docTypeHtml $
    do H.head $ do
         H.title $ pageTitle p
         pageHead p
       H.body $ do
         H.h1 $ pageTitle p
         pageContent p

showEdit :: FilePath -> Snap Page
showEdit fn = do
  c <- liftIO $ T.readFile fn `catch` \e ->
       if isDoesNotExistError e
       then return ""
       else ioError e

  let content = (H.form ! A.method "post") $ do
                  (H.textarea
                   ! A.rows "50"
                   ! A.cols "80"
                   ! A.name "content") $ H.text c
                  H.br
                  H.input ! A.type_ "submit" ! A.value "Save"

  return $ (page $ H.string fn) { pageContent = content }
