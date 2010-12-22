{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Applicative ( (<|>) )
import Alpacas
import Alpacas.Page ( respondPage, renderPage, modifyBody, addCss, Stylesheet(..) )
import Alpacas.EditConfig ( editConfig )
import Alpacas.ReadWriteFile ( editFile )
import Alpacas.Types ( Config(..) )
import qualified Config.Dyre as Dyre
import Snap.Types
import Snap.Util.FileServe ( fileServe )
import System.FilePath ( (</>) )
import Text.Blaze.Html5 ( (!) )
import Text.Blaze.Renderer.Utf8 ( renderHtml )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text as T

app :: Dyre.Params Config -> Config -> Snap ()
app params cfg =
    ifTop (respondPage (renderer cfg) (statusPage cfg))<|>
    route [ ("reload", reloadServer)
          , ("edit-config", editConfig params (renderer cfg))
          , ("edit-css", editFile (pfx </> "css" </> "default.css") (renderer cfg))
          , ("css", fileServe $ pfx </> "css")
          , ( "js"
            , fileServe (pfx </> "js") <|>
              fileServe (pfx </> "codemirror" </> "js")
            )
          ]

pfx :: FilePath
pfx = "/home/j3h/mine/alpacas/"

ifx :: String -> FilePath -> String
ifx s fn = showString s $ pfx </> fn

ghcOpts :: [String]
ghcOpts = [ "-i" `ifx` "src"
          , "-no-user-package-conf"
          , "-package-conf=" `ifx` "cabal-dev/packages-6.12.3.conf"
          ]

main :: IO ()
main = alpacasMain ghcOpts $ \p ->
       (defaultConfig p)
       { cfgApp = app p
       , renderer = render
       }
    where
      render = renderPage . addCss css . modifyBody addNav
      addNav h = navControls navItems >> h
      navItems = NavItem "/" "home" :
                 NavItem "/edit-css" "edit css" :
                 NavItem "/edit-config" "edit config" :
                 NavItem "/reload" "reload server" : []
      css = StylesheetLink "/css/default.css"
