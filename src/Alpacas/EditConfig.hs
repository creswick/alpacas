module Alpacas.EditConfig
    ( editConfig )
where

import Control.Monad.IO.Class ( liftIO )
import Alpacas.ReadWriteFile  ( editFile )
import Config.Dyre.Paths      ( getPaths )
import Config.Dyre.Params     ( Params )
import Alpacas.Types          ( Config )
import Snap.Types             ( Snap )
import Alpacas.Page           ( Renderer )

editConfig :: Params Config -> Renderer -> Snap ()
editConfig params r = do
  (_,_,configFile,_) <- liftIO $ getPaths params
  editFile configFile r