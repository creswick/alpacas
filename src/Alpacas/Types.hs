module Alpacas.Types
    ( Config(..) )
where

import Snap.Types ( Snap )
import Alpacas.Page ( Renderer )
import qualified Snap.Http.Server as Snap ( Config )

data Config =
    Config
    { message :: String
    , errorMsg :: Maybe String
    , cfgApp :: Config -> Snap ()
    , cfgServer :: Snap.Config Snap ()
    , renderer :: Renderer
    }
