{-# LANGUAGE BangPatterns #-}
module Wire.Event where

import Wire.Core
import Control.Applicative

eventOnChange :: (Monad m, Eq b) => Wire m a b -> Wire m a (Maybe b)
eventOnChange = eventOnChange' Nothing
    where eventOnChange' Nothing w = Wire $ \dt a -> do
              Output !b w' <- stepWire w dt a
              return $ Output Nothing $ eventOnChange' (Just b) w'
          eventOnChange' (Just b) w = Wire $ \dt a -> do
              Output !b' w' <- stepWire w dt a
              let ev = if b' == b then Nothing else Just b'
              return $ Output ev $ eventOnChange' (Just b') w'

holdWith :: Monad m => b -> Wire m a (Maybe b) -> Wire m a b
holdWith = flip hold

hold :: Monad m => Wire m a (Maybe b) -> b -> Wire m a b
hold w def = Wire $ \dt x -> do
    Output mb w' <- stepWire w dt x
    return $ case mb of
        Nothing -> Output def $ hold w' def
        Just !b -> Output b $ hold w' b

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
