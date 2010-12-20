module Alpacas.Types
    ( Config(..) )
where

import Snap.Types ( Snap )

data Config =
    Config
    { message :: String
    , errorMsg :: Maybe String
    , cfgApp :: Config -> Snap ()
    }
