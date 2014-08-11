module Wire.Tween where

import Wire.Core
import Control.Applicative

linear :: (Monad m, Fractional b) => b -> b -> Time -> Wire m a b
linear start end dur = c * t + b
    where c = pure $ end - start
          t = timeAsPercentOf dur ~> pureWire realToFrac
          b = pure start

timeAsPercentOf :: Monad m => Time -> Wire m a Double
timeAsPercentOf t = liftA2 min 1 (time / pure t)
