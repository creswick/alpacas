{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, ScopedTypeVariables #-}
module Alpacas where

import Config.Dyre.Relaunch
import Control.Applicative ( (<|>), (<$>) )
import Control.Concurrent.MVar ( newEmptyMVar, tryPutMVar, tryTakeMVar )
import GHC.Conc ( ThreadStatus(..), threadStatus )
import Control.Concurrent ( myThreadId, threadDelay, killThread )
import Control.Exception ( throwIO
                         , AsyncException(ThreadKilled)
                         , SomeException
                         , evaluate
                         )
import Control.Monad ( unless, when )
import Control.Monad.CatchIO ( catch )
import Control.Monad.IO.Class ( liftIO, MonadIO )
import Data.Maybe ( fromMaybe )
import Prelude hiding ( catch )
import Alpacas.Page  ( respondPage, Page(..), noHtml, renderPage, modifyBody, page, Renderer, scriptToHtml, appendBody, addCss, AlterPage, Stylesheet(..), addScript, Script(..) )
import Alpacas.ReadWriteFile ( editFileWithDefault, editFile, loadFileContent )
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
import Data.Version ( showVersion )
import Paths_alpacas ( getDataDir, version )

data State  = State { bufferLines :: [String] } deriving (Read, Show)

defaultApp :: Dyre.Params Config -> Config -> IO () -> Snap ()
defaultApp params cfg reloadServer = do
  dataDir <- liftIO getDataDir
  let r = renderer cfg . defaultStyles
      r' = r . addEditJS
      choice = foldr1 (<|>)
  choice [ ifTop $ respondPage r' $ statusPage cfg

         , path "reload" $ do
           liftIO reloadServer
           redirect "http://localhost:8000/"

         , path "find-file" $ editFileParam r'

         , path "load-file" $ loadFileParam

         , do q <- liftIO $ configPath params
              let editPfx fn = editFileWithDefault (q </> fn) (dataDir </> fn) >>= respondPage r'

              choice [ path "edit-config" $ editPfx "alpacas.hs"

                     , path "edit-css"    $ editPfx $ "css" </> "default.css"

                     , path "edit-js"     $ editPfx $ "js" </> "alpacas.js"

                     , dir "css" $
                       choice [ fileServe $ q </> "css"
                              , fileServe $ dataDir </> "examples" </> "j3h" </> "css"
                              ]

                     , dir "js"           $ fileServe $ q </> "js"
                     ]
         ]

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

loadFileParam :: Snap ()
loadFileParam = do
  fn <- requireParam "filename"
  c <- liftIO $ loadFileContent (T.unpack $ E.decodeUtf8 fn)
  writeText $ fromMaybe "TODO 404?" c

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
      cfg = Config ("ALPACAS v" ++ showVersion version) Nothing (defaultApp params) s render
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

realMain :: Snap.Config Snap () -> (IO () -> Snap ()) -> IO ()
realMain serverCfg mkApp = do
  mainThread <- myThreadId
  killerVar <- newEmptyMVar
  let reload = do
        amKiller <- tryPutMVar killerVar =<< myThreadId
        when amKiller $ killThread mainThread
      app = mkApp reload
      waitTimeout = 1000 -- msec
      pollInterval = 100 -- msec
      waitForThread t k = go (t `div` pollInterval)
        where
          go 0 = return ()
          go n = do
            -- XXX: Eventually, this may cause a problem, since the
            -- ThreadId may be for a thread that's already been
            -- garbage collected. The GHC documentation says that
            -- holding on to a ThreadId will keep the thread alive for
            -- now, but that will change later on.
            st <- threadStatus k
            case st of
              ThreadDied -> return ()
              ThreadFinished -> return ()
              _ -> do
                threadDelay (pollInterval * 1000)
                go $ n - 1
  Snap.httpServe serverCfg app `catch` \e ->
    case e of
      ThreadKilled -> do
        mKillerId <- tryTakeMVar killerVar
        case mKillerId of
          Nothing -> return ()
          Just killerId -> waitForThread waitTimeout killerId
        putStrLn "Reloading server"
        relaunchMaster Nothing
      _ -> throwIO e

-- | XXX: add GHC parameters to add the right location
-- There's a bootstrapping problem with the GHC options
alpacasMain :: (Dyre.Params Config -> Config) -> IO ()
alpacasMain cfg = do params' <- initializeConfiguration params
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
  (_,customBinaryName,cfgFileName,_) <- Dyre.getPaths p
  let cachePath = takeDirectory customBinaryName
  let cfgPath = takeDirectory cfgFileName
  mapM_ (createDirectoryIfMissing True) [cfgPath, cachePath]
  mGhcOpts <- setGHCOpts cfgPath
  return p { Dyre.ghcOpts = fromMaybe (Dyre.ghcOpts p) mGhcOpts }

setGHCOpts :: FilePath -> IO (Maybe [String])
setGHCOpts cfgPth =
  let loadOpts = Just `fmap` (loadGHCOpts $ cfgPth </> "ghc-opts")
      onErr e  = do
        putStrLn $ "Error loading GHC options: " ++ show e
        unless (isDoesNotExistError e) (ioError e)
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
