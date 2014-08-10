{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Wire.Core where

import Control.Applicative
import Control.Monad

(~>) :: Monad m => Wire m a b -> Wire m b c -> Wire m a c
(~>) w1 w2 = Wire $ \dt a -> do Output !b w1' <- stepWire w1 dt a
                                Output !c w2' <- stepWire w2 dt b
                                return $ Output c $ w1' ~> w2'

startingWith :: Monad m => Wire m a (Maybe b) -> b -> Wire m a b
startingWith = holdWith

holdWith :: Monad m => Wire m a (Maybe b) -> b -> Wire m a b
holdWith w def = Wire $ \dt x -> do Output mb w' <- stepWire w dt x
                                    return $ case mb of
                                        Nothing -> Output def $ holdWith w' def
                                        Just !b -> Output b $ holdWith w' b

at :: Monad m => Time -> Wire m a (Maybe a)
at t = Wire $ \dt x ->
    let !t' = t - dt in
    return $ if t' <= 0
      then Output (Just x) (constWire Nothing)
      else Output Nothing (at t')

time :: Monad m => Wire m a Time
time = timeFrom 0

timeFrom :: Monad m => Double -> Wire m a Time
timeFrom t = Wire $ \dt _ ->
    let t' = t + dt in
    return $ Output t' (timeFrom t')

instance Monad m => Applicative (Wire m a) where
    pure = constWire
    wf <*> wa = Wire $ \dt a -> do Output !f wf' <- stepWire wf dt a
                                   Output !b wa' <- stepWire wa dt a
                                   return $ Output (f b) (wf' <*> wa')

constWire :: Monad m => b -> Wire m a b
constWire b = pureWire $ \_ _ -> b

pureWire :: Monad m => (Time -> a -> b) -> Wire m a b
pureWire f = Wire $ \dt a -> return $ Output (f dt a) (pureWire f)

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
