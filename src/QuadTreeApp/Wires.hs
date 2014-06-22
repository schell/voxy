{-# LANGUAGE Arrows #-}
module QuadTreeApp where

import           Prelude hiding ((.), id, until)
import           Urza as U
import           FRP.Netwire hiding (app)
import           Control.Wire.Core
import qualified Urza.Color as Color
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)
import           System.Directory
import           System.Random
import           System.FilePath ((</>))
import           Debug.Trace
import           Urza.Data.QTree as QT
import           Types
import           Render

appWire :: InputWire App App
appWire = app . (captureRect <|> pass) . drawManyRects
    where app = App <$> arr appInputRect
                    <*> qtreeWire . arr appTree
                    <*> pass . arr appColors
                    <*> collisions <|> pass
          captureRect = proc (App ir t (c:cs) cols) -> do
                            _ <- useNow . mouseUpLeft -< ()
                            let Rectangle x y w h = ir
                                [x',y',w',h'] = map (fromIntegral . round) [x,y,w,h]
                                ir' = Rectangle x' y' w' h'
                            returnA -< App zeroRect (insert ir' (ir', c) t) cs cols
          collisions = proc app@(App _ t _ _) -> do
                           p <- useNow . cursorMoveEvent <|> pure (0,0) -< ()
                           returnA -< queryPoint p t

qtreeWire :: InputWire AppTree AppTree
qtreeWire = rToResetTree        <|>
            --leftMouseDeleteCols <|>
            updateQTreeRootSize <|>
            pass

rToResetTree :: InputWire AppTree AppTree
rToResetTree = proc (QTree bb _ _) -> do
    useNow_ . onCharEvent 'r' -< ()
    returnA -< (QTree bb Nothing [])

--leftMouseDeleteCols :: InputWire AppTree AppTree
--leftMouseDeleteCols = proc qt -> do
--    useNow_ . mouseButtonEvent MouseButton'2 MouseButtonState'Released -< ()
--    returnA -<

updateQTreeRootSize :: InputWire AppTree AppTree
updateQTreeRootSize = proc t@(QTree (Rectangle x y _ _) _ _) -> do
    (ww,wh) <- arr (\(a, b) -> (fromIntegral a, fromIntegral b)) . useNow . windowResizeEvent -< ()
    returnA -< foldl (\t' (QLeaf bb a) -> insert bb a t') (QT.empty (Rectangle x y ww wh)) $ leaves t


getOneBox :: InputWire () BoundingBox
getOneBox = proc _ -> do
    (x,y)   <- asSoonAs . mouseButtonEvent MouseButton'1 MouseButtonState'Pressed -< ()
    (dx,dy) <- asSoonAs . cursorMoveEvent . arr (const ()) <|> pass -< (x,y)
    mouseup <- mouseButtonEvent MouseButton'1 MouseButtonState'Released -< ()
    until   -< (absRect $ Rectangle x y (dx - x) (dy - y), mouseup)

drawOneRect :: InputWire App App
drawOneRect = proc app -> do
    r <- getOneBox -< ()
    returnA -< app { appInputRect = r }

drawManyRects :: InputWire App App
drawManyRects = pass >-- drawOneRect --> drawManyRects

-- Animate back and forth horizontally.
tweenPosWire :: InputWire () Position
tweenPosWire = Position <$> tween <*> tween
    where tween1 = for 1 . fmap round (easeInOutExpo (0 :: Double) 200 1)
          tween2 = for 1 . fmap round (easeInOutExpo (200 :: Double) 0 1)
          tween  = tween1 --> tween2 --> tween

