module Alpacas.Types
    ( Config(..) )
where

import Snap.Types ( Snap )
import Alpacas.Page ( Renderer )

data Config =
    Config
    { message :: String
    , errorMsg :: Maybe String
    , cfgApp :: Config -> Snap ()
    , renderer :: Renderer
    }
