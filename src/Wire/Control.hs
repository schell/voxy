{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Wire.Control (
    -- * Switching
    switchWhen,
    (-->),
    -- * Chaining
    chainWhen
) where

import Wire.Core

--------------------------------------------------------------------------------
-- Switching
--------------------------------------------------------------------------------

-- | Acts as `w1` until `p` evaluates true on the output of `sw`, then acts
-- as `w2`.
-- To switch from `pure 3` to `pure 4` after 4 seconds:
-- @
-- switchWhen (at 4) isJust
--   (pure 3) (pure 4)
-- @
switchWhen :: Monad m
           => Wire m a c  -- ^ the switching wire.
           -> (c -> Bool) -- ^ The switching predicate.
           -> Wire m a b  -- ^ The initial wire.
           -> Wire m a b  -- ^ the wire switched to.
           -> Wire m a b
switchWhen sw p w1 w2 = chainWhen sw p w1 (const w2)

-- | Acts as `fmap fromJust w1` until `w1` produces `Nothing`, then switches
-- to `w2`.
(-->) :: Monad m => Wire m a (Maybe b) -> Wire m a b -> Wire m a b
w1 --> w2 = Wire $ \dt a -> do
    Output mb w1' <- stepWire w1 dt a
    case mb of
        Nothing -> stepWire w2 dt a
        Just !b -> return $ Output b (w1' --> w2)

--------------------------------------------------------------------------------
-- Chaining
--------------------------------------------------------------------------------

-- | Acts as `w1` until `p` evaluates true on the output of `sw`, then
-- feeds `fw` the last output of `w1` to produce the new wire. This allows
-- a second wire to start up where the last one left off.
chainWhen :: Monad m
          => Wire m a c         -- ^ The switching wire.
          -> (c -> Bool)        -- ^ The switching predicate.
          -> Wire m a b         -- ^ The initial wire.
          -> (b -> Wire m a b)  -- ^ A function to generate the next wire.
          -> Wire m a b
chainWhen = chainWhen' Nothing
    where chainWhen' Nothing sw p w fw = Wire $ \dt a -> do
              Output !b w'  <- stepWire w dt a
              Output !c sw' <- stepWire sw dt a
              if p c
                then stepWire (fw b) dt a
                else return $ Output b $ chainWhen' (Just b) sw' p w' fw
          chainWhen' (Just b) sw p w fw = Wire $ \dt a -> do
              Output !c sw' <- stepWire sw dt a
              if p c
                then stepWire (fw b) dt a
                else do Output !b' w'  <- stepWire w dt a
                        return $ Output b' (chainWhen' (Just b') sw' p w' fw)

