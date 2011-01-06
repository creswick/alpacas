{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Alpacas
import Alpacas.Types
import Alpacas.Page ( appendBody )
import System.FilePath ( (</>) )

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
           let cfg = defaultConfig p
           in cfg { renderer = renderer cfg }