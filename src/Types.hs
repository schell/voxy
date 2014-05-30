module Types where

import           Urza
import           FRP.Netwire
import           Control.Monad.Reader

type Step s w b = (s, w, b)

type UrzaWire s b = Wire s () (ReaderT Env IO) () b
