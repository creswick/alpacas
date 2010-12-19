module Main (main) where

import Heart

pfx :: ShowS
pfx = showString "/home/j3h/mine/heart/"

ifx :: String -> ShowS
ifx s = showString s . pfx

ghcOpts :: [String]
ghcOpts = [ "-i" `ifx` "src"
          , "-no-user-package-conf"
          , "-package-conf=" `ifx` "cabal-dev/packages-6.12.3.conf"
          ]

main :: IO ()
main = heartMain ghcOpts defaultConfig
