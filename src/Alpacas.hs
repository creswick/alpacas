{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Alpacas where

import Config.Dyre.Relaunch
import Control.Applicative ( (<|>) )
import Control.Concurrent ( throwTo, myThreadId, forkIO, threadDelay )
import Control.Exception ( throwIO
                         , Exception(..)
                         , AsyncException(UserInterrupt)
                         )
import Control.Monad.IO.Class ( liftIO, MonadIO )
import Prelude hiding ( catch )
import Alpacas.Page  ( respondPage, Page(..), noHtml, renderPage, modifyBody )
import Alpacas.Types ( Config(..) )
import Alpacas.Server ( emptyServerConfig, server
                      , ServerConfig(error500Handler) )
import Alpacas.EditConfig ( editConfig )
import Snap.Types
import Snap.Util.FileServe
import Text.Blaze.Html5 ( (!) )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Config.Dyre as Dyre
import qualified Data.Text as T

data State  = State { bufferLines :: [String] } deriving (Read, Show)

defaultApp :: Dyre.Params Config -> Config -> Snap ()
defaultApp params cfg =
    ifTop (respondPage (renderer cfg) (statusPage cfg))<|>
    route [ ("reload", reloadServer)
          , ("edit-config", editConfig params (renderer cfg))
          ] <|>
    fileServe "."

defaultConfig :: Dyre.Params Config -> Config
defaultConfig params =
    Config "ALPACAS v0.1" Nothing (defaultApp params) render
        where
          render = renderPage . modifyBody addNav
          addNav h = navControls defaultNavItems >> h

showError :: Config -> String -> Config
showError cfg msg = cfg { errorMsg = Just msg }

realMain :: Snap () -> IO ()
realMain app = do
  mainThread <- myThreadId
  let serverCfg = emptyServerConfig
      errHandler e =
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
            _ -> error500Handler serverCfg e

      serverCfg' = serverCfg { error500Handler = errHandler }

  server serverCfg' app
  putStrLn "Reloading server"
  relaunchMaster Nothing

-- | XXX: add GHC parameters to add the right location
-- There's a bootstrapping problem with the GHC options
alpacasMain :: [String] -> (Dyre.Params Config -> Config) -> IO ()
alpacasMain ghcOpts getCfg = Dyre.wrapMain params cfg
    where
      cfg = getCfg params
      params = Dyre.defaultParams
               { Dyre.projectName = "alpacas"
               , Dyre.realMain    = \cfg1 -> realMain $ cfgApp cfg1 cfg1
               , Dyre.showError   = showError
               , Dyre.ghcOpts     = ghcOpts
               , Dyre.forceRecomp = True
               }

navControls :: [NavItem] -> H.Html
navControls = (H.ul ! A.class_ "navigation") . mapM_ mkNavItem
    where
      mkNavItem (NavItem pth lbl) = H.li $ H.a ! A.href pth $ H.text lbl

data NavItem = NavItem H.AttributeValue T.Text

defaultNavItems :: [NavItem]
defaultNavItems = [ "/reload" |-| "reload server"
                  , "/edit-config" |-| "edit configuration"
                  ]
    where
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
