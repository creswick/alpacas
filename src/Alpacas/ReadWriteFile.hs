{-# LANGUAGE OverloadedStrings #-}
module Alpacas.ReadWriteFile where

import Control.Applicative      ( (<|>) )
import Control.Monad.IO.Class   ( liftIO )
import Data.String              ( fromString )
import Snap.Types
import System.IO.Error          ( isDoesNotExistError )
import Text.Blaze.Html5         ( (!) )
import System.FilePath          ( takeDirectory )
import System.Directory         ( createDirectoryIfMissing )
import Alpacas.Page             ( Page(..), respondPage, page, Renderer )
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString             as B

editFile :: FilePath -> Renderer -> Snap ()
editFile fn r = do
  method GET (return ()) <|> method POST (doUpdate fn)
  respondPage r =<< liftIO (showEdit fn)

-- XXX: redirect to show action instead of showing again
-- XXX: this requires the URL to the page
doUpdate :: FilePath -> Snap ()
doUpdate fn = do
  contentVal <- getParam "content"
  liftIO $ case contentVal of
             Nothing -> return ()
             Just v  ->
                 do let d = takeDirectory fn
                    createDirectoryIfMissing True d
                    B.writeFile fn v

data EditForm =
    EditForm { efContent  :: Maybe T.Text
             , efFileName :: FilePath
             , efAction   :: Maybe T.Text
             }

renderEditForm :: EditForm -> H.Html
renderEditForm ef =
    formTag $ do
      textArea $ H.text $ maybe T.empty id $ efContent ef
      H.br
      H.input ! A.type_ "submit" ! A.value "Save"
    where
      textArea = H.textarea ! A.rows "50" ! A.cols "80" ! A.name "content"
      formTag = action $ H.form ! A.method "post"
      action = maybe id (\u -> (! A.action (fromString $ T.unpack u))) $ efAction ef

loadFileContent :: FilePath -> IO (Maybe T.Text)
loadFileContent fn =
    (Just `fmap` T.readFile fn) `catch` \e ->
        if isDoesNotExistError e
        then return Nothing
        else ioError e

showEdit :: FilePath -> IO Page
showEdit fn = do
  c <- liftIO $ loadFileContent fn
  let f = renderEditForm $ EditForm c fn Nothing
  return $ (page $ H.string fn) { pageContent = f }
