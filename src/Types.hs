{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import           Urza as U hiding (fill, stroke)
import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Identity


type Render a = a -> IO a

newtype App a b = App {
    runA :: ReaderT Env (StateT a Identity) b
} deriving (Applicative, Functor, Monad, MonadReader Env, MonadState a)

data Consumption a = Consumption { tscApp :: a
                                 , tscEnv :: Env
                                 }

withInputEnv :: Env -> InputEnv -> Env
withInputEnv e i = e{ inputEnv = i }

withAppEnv :: Env -> AppEnv -> Env
withAppEnv e a = e{ appEnv = a }


data Env = Env { inputEnv :: InputEnv
               , appEnv   :: AppEnv
               }

withAETime :: AppEnv -> Double -> AppEnv
withAETime a t = a{ aeTime = t}

withAEDelta :: AppEnv -> Double -> AppEnv
withAEDelta a t = a{ aeDelta = t}

withAEDeltaAcc :: AppEnv -> Double -> AppEnv
withAEDeltaAcc a t = a{ aeDeltaAcc = t}


data AppEnv = AppEnv  { aeTime     :: Double
                      -- ^ The total number of gametime seconds passed.
                      , aeDeltaAcc :: Double
                      -- ^ An accumulator for building up enough dt for
                      -- a game step.
                      , aeDelta    :: Double
                      -- ^ The dt of a game step.
                      } deriving (Show, Eq)



