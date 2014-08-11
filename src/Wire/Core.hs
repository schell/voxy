{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Wire.Core where

import Prelude hiding (id, (.))
import Control.Applicative
import Control.Category
import Control.Monad
import Data.Time.Clock

startingWith :: Monad m => Wire m a (Maybe b) -> b -> Wire m a b
startingWith = holdWith

holdWith :: Monad m => Wire m a (Maybe b) -> b -> Wire m a b
holdWith w def = Wire $ \dt x -> do Output mb w' <- stepWire w dt x
                                    return $ case mb of
                                        Nothing -> Output def $ holdWith w' def
                                        Just !b -> Output b $ holdWith w' b

(-->) :: Monad m => Wire m a (Maybe b) -> Wire m a b -> Wire m a b
w1 --> w2 = Wire $ \dt a -> do
    Output mb w1' <- stepWire w1 dt a
    case mb of
        Nothing -> stepWire w2 dt a
        Just !b -> return $ Output b (w1' --> w2)

for :: Monad m => Time -> Wire m a b -> Wire m a (Maybe b)
for t w = Wire $ \dt a ->
    let !t' = t - dt in
    if t' <= 0
      then return $ Output Nothing (pure Nothing)
      else do Output !b w' <- stepWire w dt a
              return $ Output (Just b) (for t' w')

at :: Monad m => Time -> Wire m a (Maybe a)
at t = Wire $ \dt x ->
    let !t' = t - dt in
    return $ if t' <= 0
      then Output (Just x) (pure Nothing)
      else Output Nothing (at t')

time :: Monad m => Wire m a Time
time = timeFrom 0

timeFrom :: Monad m => Double -> Wire m a Time
timeFrom t = Wire $ \dt _ ->
    let t' = t + dt in
    return $ Output t' (timeFrom t')

instance Monad m => Applicative (Wire m a) where
    pure = pureWire . const
    wf <*> wa = Wire $ \dt a -> do Output !f wf' <- stepWire wf dt a
                                   Output !b wa' <- stepWire wa dt a
                                   return $ Output (f b) (wf' <*> wa')

instance (Monad m, Floating b) => Floating (Wire m a b) where
    pi = pure pi
    exp = fmap exp
    log = fmap log
    sin = fmap sin
    cos = fmap cos
    asin = fmap asin
    sinh = fmap sinh
    cosh = fmap cosh
    atan = fmap atan
    acos = fmap acos
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acosh

instance (Monad m, Fractional b) => Fractional (Wire m a b) where
    (/) = liftA2 (/)
    fromRational = pure . fromRational

instance (Monad m, Num b) => Num (Wire m a b) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Monad m => Category (Wire m) where
    id  = pureWire id
    (.) = (<~)

-- | Plugs the output value of w2 into the input value of w1.
(<~) :: Monad m => Wire m b c -> Wire m a b -> Wire m a c
(<~) = flip (~>)

-- | Plugs the output value of w1 into the input value of w2.
(~>) :: Monad m => Wire m a b -> Wire m b c -> Wire m a c
(~>) w1 w2 = Wire $ \dt a -> do Output !b w1' <- stepWire w1 dt a
                                Output !c w2' <- stepWire w2 dt b
                                return $ Output c $ w1' ~> w2'

pureWire :: Monad m => (a -> b) -> Wire m a b
pureWire = timeWire . const

timeWire :: Monad m => (Time -> a -> b) -> Wire m a b
timeWire f = Wire $ \dt a -> return $ Output (f dt a) (timeWire f)

testWire :: Show b => Wire IO () b -> IO b
testWire w = do
    t <- getCurrentTime
    loopWire t w
        where loopWire t' w' = do t'' <- getCurrentTime
                                  let dt = realToFrac $ diffUTCTime t'' t'
                                  Output b w'' <- stepWire w' dt ()
                                  print b
                                  loopWire t'' w''

execWire :: (Monad m, Functor m) => Wire m a b -> Time -> a -> m b
execWire w dt a = fmap outVal $ stepWire w dt a

stepWire :: Monad m => Wire m a b -> Time -> a -> m (Output m a b)
stepWire (Wire !w) dt !a = w dt a

instance Monad m => Functor (Wire m a) where
    fmap f (Wire w) = Wire $ \dt a -> liftM (fmap f) $ w dt a

instance Monad m => Functor (Output m a) where
    fmap f (Output val w) = Output (f val) (fmap f w)

data Output m a b = Output { outVal  :: !b
                           , outWire :: !(Wire m a b)
                           }

data Wire m a b = Wire (Time -> a -> m (Output m a b))

type Time = Double
