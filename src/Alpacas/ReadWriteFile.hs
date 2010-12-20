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
editFile fn = method GET (showEdit fn) <|>
              method POST (doUpdate fn)

-- XXX: redirect to show action instead of showing again
doUpdate :: FilePath -> Snap ()
doUpdate fn = do
  contentVal <- getParam "content"
  case contentVal of
    Nothing -> showEdit fn
    Just v  ->
        do let d = takeDirectory fn
           liftIO $ createDirectoryIfMissing True d
           liftIO $ B.writeFile fn v
           showEdit fn

showEdit :: FilePath -> Snap ()
showEdit fn = do
  modifyResponse $ setContentType "text/html"
  c <- liftIO $ T.readFile fn `catch` \e ->
       if isDoesNotExistError e
       then return ""
       else ioError e
  writeLBS $ renderHtml $ H.docTypeHtml $
    do H.head $ H.title $ H.string fn
       H.body $ do
         H.h1 $ H.string fn
         (H.form ! A.method "post") $ do
                  (H.textarea
                   ! A.rows "50"
                   ! A.cols "80"
                   ! A.name "content") $ H.text c
                  H.br
                  H.input ! A.type_ "submit" ! A.value "Save"
