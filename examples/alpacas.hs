-- j3h's ALPACAS configuration

{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Applicative ( (<|>) )
import Alpacas hiding ( statusPage )
import Alpacas.EditConfig ( editConfig )
import Alpacas.Types ( Config(..) )
import qualified Config.Dyre as Dyre
import Snap.Types
import Snap.Util.FileServe ( fileServe )
import Text.Blaze.Html5 ( (!) )
import Text.Blaze.Renderer.Utf8 ( renderHtml )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text as T

app :: Dyre.Params Config -> Config -> Snap ()
app params cfg =
    ifTop (statusPage cfg) <|>
    route [ ("reload", reloadServer)
          , ("edit-config", editConfig params)
          ] <|>
    fileServe "."

pfx :: ShowS
pfx = showString "/home/j3h/mine/alpacas/"

ifx :: String -> ShowS
ifx s = showString s . pfx

ghcOpts :: [String]
ghcOpts = [ "-i" `ifx` "src"
          , "-no-user-package-conf"
          , "-package-conf=" `ifx` "cabal-dev/packages-6.12.3.conf"
          ]

main :: IO ()
main = alpacasMain ghcOpts $ \p -> (defaultConfig p) { cfgApp = app p }

statusPage :: Config -> Snap ()
statusPage cfg =
    let t = T.pack $ "Status - " ++ message cfg
        noError = H.p $ H.text "Started OK!"
        errorHtml e = H.pre $ H.string e
    in writeLBS $ renderHtml $ H.docTypeHtml $ do
      H.head $ H.title $ H.text t
      H.body $ do
        H.h1 $ H.text t
        H.p $ H.text "j3h's ALPACAS server"
        H.p $ do
          H.a ! A.href "/reload" $ H.text "reload"
          H.text " | "
          H.a ! A.href "/edit-config" $ H.text "edit configuration"
        maybe noError errorHtml $ errorMsg cfg
