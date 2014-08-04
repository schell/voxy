{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Time.Clock
import Control.Applicative
import Control.Concurrent
--import Control.Monad.Reader
import System.Exit
import System.Environment

main :: IO ()
main = do
    s:_  <- getArgs
    v    <- newMVar $ read s
    t    <- getCurrentTime
    loop v t $ MyRecord <$> timeVaryingInteger <*> timeVaryingString

loop :: Show b => MVar Double -> UTCTime -> Wire () b -> IO ()
loop v t w = do
    t' <- getCurrentTime
    let dt = realToFrac $ diffUTCTime t' t
    let Output t'' w' = stepWire w dt ()
    print t''
    tleft <- takeMVar v
    let tleft' = tleft - dt
    if tleft' <= 0
      then exitSuccess
      else do putMVar v tleft'
              loop v t' w'

--isZero :: Wire () Bool
--isZero = Wire $ \dt a -> do
--    e <- ask
--    return $ if e == 0
--               then Output True isZero
--               else Output False isZero

timeVaryingInteger :: Wire () Integer
timeVaryingInteger = (simply 45 ~> at 3) `startingWith` 666

timeVaryingString :: Wire () String
timeVaryingString = (simply "Done." ~> at 4) `startingWith` "Almost done."

(~>) :: Wire a b -> Wire b c -> Wire a c
(~>) = plug

plug :: Wire a b -> Wire b c -> Wire a c
plug w1 w2 = Wire $ \dt a ->
    let Output b w1' = stepWire w1 dt a
        Output c w2' = stepWire w2 dt b
    in Output c $ w1' `plug` w2'

startingWith :: Wire a (Maybe b) -> b -> Wire a b
startingWith = holdWith

holdWith :: Wire a (Maybe b) -> b -> Wire a b
holdWith w def = Wire $ \dt x ->
    let Output mb w' = stepWire w dt x in
    case mb of
        Nothing -> Output def $ holdWith w' def
        Just b  -> Output b $ holdWith w' b

at :: Time -> Wire a (Maybe a)
at t = Wire $ \dt x ->
    let !t' = t - dt in
    if t' <= 0
      then Output (Just x) (mkConst Nothing)
      else Output Nothing (at t')

time :: Wire a Time
time = timeFrom 0

timeFrom :: Double -> Wire a Time
timeFrom t = Wire $ \dt _ ->
    let t' = t + dt in
    Output t' (timeFrom t')

stepWire :: Wire a b -> Time -> a -> Output a b
stepWire (Wire !w) dt !a = w dt a

data MyRecord = MyRecord { myInteger :: Integer
                         , myString  :: String
                         } deriving (Show)

instance Applicative (Wire a) where
    pure = simply
    wf <*> wa = Wire $ \dt a -> let Output a' wa' = stepWire wa dt a
                                    Output f wf'  = stepWire wf dt a
                                in Output (f a') (wf' <*> wa')

simply :: b -> Wire a b
simply = mkConst

mkConst :: b -> Wire a b
mkConst b = Wire $ \_ _ -> Output b (mkConst b)

instance Functor (Wire a) where
    fmap f w@(Wire _) = Wire $ \dt a -> let Output b w' = stepWire w dt a
                                        in Output (f b) (fmap f w')


data Output a b = Output { outVal  :: !b
                         , outWire :: !(Wire a b)
                         }

data Wire a b = Wire (Time -> a -> Output a b)

type Time = Double

