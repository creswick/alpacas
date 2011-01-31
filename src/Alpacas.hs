{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, ScopedTypeVariables #-}
module Alpacas where

import Config.Dyre.Relaunch
import Control.Applicative ( (<|>), (<$>) )
import Control.Concurrent ( throwTo, myThreadId, forkIO, threadDelay )
import Control.Exception ( throwIO
                         , Exception(..)
                         , AsyncException(UserInterrupt)
                         , SomeException
                         , evaluate
                         )
import Control.Monad ( unless )
import Control.Monad.CatchIO ( throw, catch )
import Control.Monad.IO.Class ( liftIO, MonadIO )
import Data.Maybe ( fromMaybe )
import Prelude hiding ( catch )
import Alpacas.Page  ( respondPage, Page(..), noHtml, renderPage, modifyBody, page, Renderer, scriptToHtml, appendBody, addCss, AlterPage, Stylesheet(..), addScript, Script(..) )
import Alpacas.ReadWriteFile ( editFile )
import Alpacas.Types ( Config(..) )
import Snap.Types
import Snap.Util.FileServe
import Text.Blaze.Html5 ( (!) )
import System.Directory ( createDirectoryIfMissing )
import System.IO.Error ( isDoesNotExistError )
import System.FilePath ( (</>), takeDirectory )
import qualified Snap.Http.Server as Snap
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Config.Dyre as Dyre
import qualified Config.Dyre.Paths as Dyre
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B

data State  = State { bufferLines :: [String] } deriving (Read, Show)

defaultApp :: Dyre.Params Config -> Config -> Snap ()
defaultApp params cfg =
    let r = renderer cfg . defaultStyles
        r' = r . addEditJS
    in ifTop (respondPage r' (statusPage cfg)) <|>
       path "reload" reloadServer <|>
       path "find-file" (editFileParam r') <|>
       do q <- liftIO $ configPath params
          let editPfx fn = editFile (q </> fn) >>= respondPage r'
          path "edit-config" (editPfx "alpacas.hs") <|>
            path "edit-css" (editPfx ("css" </> "default.css")) <|>
            path "edit-js" (editPfx ("js" </> "alpacas.js")) <|>
            dir "css" (fileServe $ q </> "css") <|>
            dir "js" (fileServe (q </> "js"))

configPath :: Dyre.Params a -> IO FilePath
configPath p = do (_,_,fn,_) <- Dyre.getPaths p
                  return $ takeDirectory fn

missingParam :: Snap a
missingParam = error "Missing parameter (FIXME: should render a 400 page?)"

requireParam :: B.ByteString -> Snap B.ByteString
requireParam k = do
 mv <- getParam k
 case mv of
   Nothing -> missingParam
   Just v  -> return v

editFileParam :: Renderer -> Snap ()
editFileParam r = do
  fn <- requireParam "filename"
  editFile (T.unpack $ E.decodeUtf8 fn) >>= respondPage r

defaultStyles :: AlterPage
defaultStyles = ours . y
    where
      y = addCss $ StylesheetLink "http://yui.yahooapis.com/combo?3.2.0/build/cssreset/reset-min.css&3.2.0/build/cssfonts/fonts-min.css&3.2.0/build/cssgrids/grids-min.css"
      ours = addCss $ StylesheetLink "/css/default.css"

addEditJS :: AlterPage
addEditJS = addBodyScript . srcScripts
 where
  srcScripts = foldr (\s -> ((addScript $ ScriptSrc s) .)) id ["/js/alpacas.js"]
  addBodyScript = appendBody $ scriptToHtml $ ScriptInline
    "window.onload = ALPACAS.init;"

defaultConfig :: Dyre.Params Config -> Config
defaultConfig params = cfg
    where
      cfg = Config "ALPACAS v0.1" Nothing (defaultApp params) s render
      render = renderPage . modifyBody addNav
      addNav h = navControls defaultNavItems >> h
      s = Snap.setErrorHandler (error500Handler cfg) Snap.defaultConfig

showError :: Config -> String -> Config
showError cfg msg = cfg { errorMsg = Just msg }

error500Handler :: Config -> SomeException -> Snap ()
error500Handler cfg e = do
  let r = setContentType "text/html; charset=utf-8" $
          setResponseStatus 500 "Internal Server Error" emptyResponse
      t = "Internal Server Error"
      c = do
        H.h1 t
        H.p "A web handler threw an exception. Details:"
        H.pre $ H.string $ show e
  putResponse r
  respondPage (renderer cfg) (page t) { pageContent = c }

realMain :: Snap.Config Snap () -> Snap () -> IO ()
realMain serverCfg app = do
  mainThread <- myThreadId
  let errHandler e =
          case fromException e of
            -- FIXME: We are using UserInterrupt here because it's one
            -- of the few exceptions that Snap will allow to bubble up
            -- and kill the server. Ideally, we would have an
            -- exception type that's specifically for stopping the
            -- server, distinguishable from UserInterrupt. This works
            -- for now, because if you send it ^C twice, it will
            -- likely not be back in this handler yet.
            Just UserInterrupt ->
                do -- FIXME: this is a very crude way of attempting to
                   -- allow a response for this action to be received
                   -- by the client before the server re-loads
                   _ <- liftIO $ forkIO $ do threadDelay 10000
                                             throwTo mainThread e
                   -- FIXME: Fix this hard-coded URL
                   redirect "http://localhost:8000/"
            _ -> maybe throw id (Snap.getErrorHandler serverCfg) e

      serverCfg' = Snap.setErrorHandler errHandler serverCfg

  Snap.httpServe serverCfg' app `catch` \(e::SomeException) -> print e
  putStrLn "Reloading server"
  relaunchMaster Nothing

-- | XXX: add GHC parameters to add the right location
-- There's a bootstrapping problem with the GHC options
alpacasMain :: (Dyre.Params Config -> Config) -> IO ()
alpacasMain cfg = do
  params' <- initializeConfiguration params
  Dyre.wrapMain params' $ cfg params'
    where
      params = Dyre.defaultParams
               { Dyre.projectName = "alpacas"
               , Dyre.realMain    = \cfg1 -> realMain (cfgServer cfg1) $
                                    cfgApp cfg1 cfg1
               , Dyre.showError   = showError
               , Dyre.ghcOpts     = []
               , Dyre.forceRecomp = True
               }

initializeConfiguration :: Dyre.Params a -> IO (Dyre.Params a)
initializeConfiguration p = do
  cfgPth <- configPath p
  createDirectoryIfMissing True cfgPth
  mGhcOpts <- setGHCOpts cfgPth
  return p { Dyre.ghcOpts = fromMaybe (Dyre.ghcOpts p) mGhcOpts }

setGHCOpts :: FilePath -> IO (Maybe [String])
setGHCOpts cfgPth =
  let loadOpts = Just `fmap` (loadGHCOpts $ cfgPth </> "ghc-opts")
      onErr e  = do
        putStrLn $ "Error loading GHC options: " ++ show e
        unless (isDoesNotExistError e) (ioError e)
        putStrLn $ "Options file does not exist: " ++ show e
        return Nothing
  in loadOpts `catch` onErr

loadGHCOpts :: FilePath -> IO [String]
loadGHCOpts ghcOptsFile = do
  let clean = reverse . dropWhile (== '\r') . reverse
  ls <- map clean . lines <$> readFile ghcOptsFile
  evaluate $ length ls
  putStrLn $ "Loaded GHC options: " ++ show ls
  return ls

navControls :: [NavItem] -> H.Html
navControls = (H.ul ! A.class_ "navigation") . mapM_ mkNavItem
    where
      mkNavItem (NavItem pth lbl) = H.li $ H.a ! A.href pth $ H.text lbl
      mkNavItem (NavCustom h) = H.li h

data NavItem = NavItem H.AttributeValue T.Text
             | NavCustom H.Html

defaultNavItems :: [NavItem]
defaultNavItems = [ "/"            |-| "home"
                  , "/edit-config" |-| "edit configuration"
                  , "/edit-css"    |-| "edit css"
                  , "/edit-js"     |-| "edit javascript"
                  , "/edit-config" |-| "edit config"
                  , "/reload"      |-| "reload server"
                  , NavCustom editFileForm
                  ]
    where
      editFileForm = H.form ! A.method "post" ! A.action "/find-file" $ do
                       H.label ! A.for "find-file-input" $ "find file: "
                       H.input ! A.type_ "text" ! A.name "filename" ! A.id "find-file-input"
      (|-|) = NavItem

statusPage :: Config -> Page
statusPage cfg =
    let t = H.text $ T.pack $ "Status - " ++ message cfg
    in Page { pageTitle = t
            , pageHead = noHtml
            , pageContent =
                case errorMsg cfg of
                  Nothing -> H.p $ H.text "Started OK!"
                  Just e  -> H.pre $ H.string e
            }

reloadServer :: MonadIO m => m a
reloadServer = liftIO $ throwIO UserInterrupt
