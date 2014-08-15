{-# LANGUAGE Rank2Types #-}
module Wire.Tween where

import Wire.Core
import Wire.Control
import Wire.Event
import Control.Applicative
import Data.Maybe
import Linear

tweenV2 :: (Monad m, Floating b)
        => (b -> b -> Time -> Wire m a b)
        -> V2 b
        -> V2 b
        -> Time
        -> Wire m a (V2 b)
tweenV2 f (V2 x1 y1) (V2 x2 y2) dur = V2 <$> (f x1 x2 dur) <*> (f y1 y2 dur)

easeInQuad :: Tween
easeInQuad = tween $ \c t b -> c * t*t + b

easeOutQuad :: Tween
easeOutQuad = tween $ \c t b -> (-c) * (t * (t - 2)) + b

easeInOutQuad :: Tween
easeInOutQuad = easeInOut easeInQuad easeOutQuad

easeInCubic :: Tween
easeInCubic = tween $ \c t b -> c * t*t*t + b

easeOutCubic :: Tween
easeOutCubic = tween $ \c t b -> let t' = t - 1 in c * (t'*t'*t' + 1) + b

easeInOutCubic :: Tween
easeInOutCubic = easeInOut easeInCubic easeOutCubic

easeInPow :: Int -> Tween
easeInPow power = tween $ \c t b -> c * (t^power) + b

easeOutPow :: Int -> Tween
easeOutPow power = tween $ \c t b ->
    let t' = t - 1
        c' = if power `mod` 2 == 1 then c else -c
        i  = if power `mod` 2 == 1 then 1 else -1
    in c' * ((t'^power) + i) + b

easeInSine :: Tween
easeInSine = tween $ \c t b -> let cos' = cos (t * (pi / 2))
                               in -c * cos' + c + b

easeOutSine :: Tween
easeOutSine = tween $ \c t b -> let cos' = cos (t * (pi / 2)) in c * cos' + b

easeInOutSine :: Tween
easeInOutSine = tween $ \c t b -> let cos' = cos (pi * t)
                                  in (-c / 2) * (cos' - 1) + b

easeInExpo :: Tween
easeInExpo = tween $ \c t b -> let e = 10 * (t - 1) in c * (2**e) + b

easeOutExpo :: Tween
easeOutExpo = tween $ \c t b -> let e = -10 * t in c * (-(2**e) + 1) + b

easeInOutExpo :: Tween
easeInOutExpo = easeInOut easeInExpo easeOutExpo

easeInCirc :: Tween
easeInCirc = tween $ \c t b -> let s = sqrt (1 - t*t) in -c * (s - 1) + b

easeOutCirc :: Tween
easeOutCirc = tween $ \c t b -> let t' = (t - 1)
                                    s  = sqrt (1 - t'*t')
                                in c * s + b

easeInOutCirc :: Tween
easeInOutCirc = easeInOut easeInCirc easeOutCirc

easeInOut :: (Monad m, Fractional a)
          => (a -> a -> Time -> Wire m c b)
          -> (a -> a -> Time -> Wire m c b)
          -> (a -> a -> Time -> Wire m c b)
easeInOut ein eout start end dur =
    let middle = start + (end - start) / 2
        up     = ein start middle (dur/2)
        down   = eout middle end (dur/2)
    in switchWhen (at $ dur/2) isJust up down

linear :: Tween
linear = tween $ \c t b -> c * t + b

type TweenDouble = (Monad m) => Double -> Double -> Time -> Wire m a Double
type Tween = (Monad m, Floating b) => b -> b -> Time -> Wire m a b

-- | Applies a tweening function `f` in order to interpolate a value from
-- `start` to `end` over `dur`.
tween :: (Applicative f', Applicative f, Num s, Monad m, Fractional c)
      => (f' s -> Wire m a c -> f s -> t) -> s -> s -> Time -> t
tween f start end dur = f c t b
    where c = pure $ end - start
          t = timeAsPercentageOf dur ~> pureWire realToFrac
          b = pure start

-- | Varies 0.0 to 1.0 linearly for duration `t` and 1.0 after `t`.
timeAsPercentageOf :: Monad m => Time -> Wire m a Double
timeAsPercentageOf t = liftA2 min 1 (time / pure t)
