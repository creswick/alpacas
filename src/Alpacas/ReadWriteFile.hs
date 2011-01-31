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
import Alpacas.Page             ( Page(..), page )
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString             as B

editFile :: FilePath -> Snap Page
editFile fn = genericEditFile fn Nothing

editFileWithDefault :: FilePath -> FilePath -> Snap Page
editFileWithDefault fn def = genericEditFile fn $ Just def

genericEditFile :: FilePath -> Maybe FilePath -> Snap Page
genericEditFile fn def
  method GET (return ()) <|> method POST (doUpdate fn)
  liftIO (showEdit fn def)

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
             , efFileName :: Maybe FilePath
             , efAction   :: Maybe T.Text
             , efDefaultContentPath :: Maybe FilePath
             }

renderEditForm :: EditForm -> H.Html
renderEditForm ef =
    formTag $ do
      textArea $ H.text $ maybe T.empty id $ efContent ef
      H.br
      maybe (return ()) fnTag $ efFileName ef
      H.input ! A.type_ "submit" ! A.value "Save"
      case efDefaultContentPath ef of
        Nothing -> return ()
        Just _  -> H.input ! A.type_ "button" ! A.value "Load Default"
    where
      textArea = H.textarea ! A.rows "50" ! A.cols "80" ! A.name "content"
      formTag = action $ H.form ! A.method "post"
      fnTag fn = H.input
                 ! A.type_ "hidden"
                 ! A.name "filename"
                 ! A.value (fromString fn)
      action = maybe id (\u -> (! A.action (fromString $ T.unpack u))) $
               efAction ef

loadFileContent :: FilePath -> IO (Maybe T.Text)
loadFileContent fn =
    (Just `fmap` T.readFile fn) `catch` \e ->
        if isDoesNotExistError e
        then return Nothing
        else ioError e

showEdit :: FilePath -> Maybe FilePath -> IO Page
showEdit fn mDef = do
  c <- liftIO $ loadFileContent fn
  let f = renderEditForm $ EditForm c (Just fn) Nothing mDef
  return $ (page $ H.string fn) { pageContent = f }
