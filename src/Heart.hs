{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Heart where

import Config.Dyre.Relaunch
import Control.Applicative ( (<|>) )
import Control.Concurrent ( throwTo, myThreadId, forkIO, threadDelay )
import Control.Exception ( throwIO
                         , Exception(..)
                         , AsyncException(UserInterrupt)
                         )
import Control.Monad.IO.Class ( liftIO, MonadIO )
import Data.Typeable ( Typeable )
import Prelude hiding ( catch )
import Server ( emptyServerConfig, server
              , ServerConfig(error500Handler) )
import Snap.Types
import Snap.Util.FileServe
import Text.XHtmlCombinators
import qualified Text.XHtmlCombinators.Attributes as A
import Text.XHtmlCombinators.Escape
import qualified Config.Dyre as Dyre
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

data ReloadException = ReloadException deriving (Show, Typeable)
instance Exception ReloadException

data Config =
    Config
    { message :: String
    , errorMsg :: Maybe String
    , cfgApp :: Config -> Snap ()
    }

data State  = State { bufferLines :: [String] } deriving (Read, Show)

defaultApp :: Config -> Snap ()
defaultApp cfg =
    ifTop (statusPage cfg) <|>
    route [ ("reload", reloadServer) ] <|>
    fileServe "."

defaultConfig :: Config
defaultConfig = Config "HEART v0.1" Nothing defaultApp

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
heartMain :: [String] -> Config -> IO ()
heartMain ghcOpts cfg = Dyre.wrapMain params cfg
    where
      params = Dyre.defaultParams
               { Dyre.projectName = "heart"
               , Dyre.realMain    = realMain . cfgApp cfg
               , Dyre.showError   = showError
               , Dyre.ghcOpts     = ghcOpts
               , Dyre.debug       = True
               , Dyre.forceRecomp = True
               }

statusPage :: Config -> Snap ()
statusPage cfg =
    let t = T.pack $ "Status - " ++ message cfg
        noError = p $ text $ escape "Started OK!"
        errorHtml e = pre $ text $ escape $ T.pack e
    in writeBS $ E.encodeUtf8 $ render $ html True $ do
      head_ $ title $ t
      body $ do
        h1 $ text $ escape t
        maybe noError errorHtml $ errorMsg cfg
        p $ a' [ A.href "/reload" ] $ text $ escape "reload"

reloadServer :: MonadIO m => m a
reloadServer = liftIO $ throwIO UserInterrupt
