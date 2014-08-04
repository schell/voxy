{-# LANGUAGE Arrows #-}
module Jake.Wires where

{-
import           Prelude hiding ((.), id, until)
import           Control.Wire
import           Control.Wire.Core
import           Jake.Types
import           Linear hiding (trace)
import           Jake.Game
import           Urza

--import           Debug.Trace

--withSPF :: (Fractional t, RealFrac t) => t -> GameWire Sprite Sprite
--withSPF fps = proc s -> do
--    t <- timeF -< ()
--    returnA -< spriteSetFrame s $ floor (t / fps) `mod` spriteFramesLength s
gameWire :: GameWire a Game
gameWire = Game <$> jakeWire <*> pure tilemap <*> windowSize (V2 400 400)


jakeWire :: GameWire a Jake
jakeWire = Jake <$> pure (JakeRunning 0)
                <*> jakePosition

jakePosition :: GameWire a Position
jakePosition = (traceWire . (V2 <$> positionX <*> positionY)) `restartOn` (traceWire . jakeCollided)


cursorPos :: GameWire a (V2 Double, V2 Double, V2 Double)
cursorPos = (,,) <$> pure (V2 0 0)
                 <*> pure (V2 0 0)
                 <*> cursorPositionStartingWith (V2 0 0)


accelerationX :: GameWire a Double
accelerationX = 0

velocityX :: GameWire a Double
velocityX = integral 0 . accelerationX

positionX :: GameWire a Double
positionX = integral 0 . velocityX

accelerationY :: GameWire a Double
accelerationY = gravity

velocityY :: GameWire a Double
velocityY = integral 0 . accelerationY

positionY :: GameWire a Double
positionY = integral 0 . velocityY




tweenPos :: Monad m => Wire m a (V2 Double, V2 Double, V2 Double)
tweenPos = (,,) <$> pure (V2 0 0)
                <*> pure (V2 0 0)
                <*> (V2 <$> cycleBetweenA (periodic 2) [ein,eout] <*> 0)
    where ein  = easeInOutExpo 0 100 2
          eout = easeInOutExpo 100 0 2



jakeCollided :: GameWire a (Event Position)
jakeCollided = mkGenA $ \_ _ -> do
    p  <- asksGame getJakesPosition
    p' <- asksGame correctJakesPosition
    return $ if p == p' then NoEvent else Event p'

--collision :: GameWire Game (Event (Double,Double))
--collision = mkGen_ $ \g ->
--    return $ Right $ case maybeCollision g of
--        Just ((x, y), _) -> Event (x,y)
--        Nothing -> NoEvent

gravity :: (Monad m) => Wire m a Double
gravity = (9*40*) <$> timeF

--asWire :: Monad m => State a () -> Wire m a a
--asWire = arr . execState
-}
