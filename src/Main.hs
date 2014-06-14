{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

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
import           QTree as QT
import           Types
import           Render

-- | Acts like the first wire until the second starts producing, at which point
-- it switches to the second wire.  Infixr 1.
--
-- * Depends: like current wire.
--
-- * Inhibits: after switching like the second wire.
--
-- * Switch: now.
(>--) :: (Monad m) => Wire s e m a b -> Wire s e m a b -> Wire s e m a b
w1' >-- w2' =
    WGen $ \ds mx' -> do
        (m2, w2) <- stepWire w2' ds mx'
        case m2 of
          Right _ -> m2 `seq` return (m2, w2)
          _       -> do (m1, w1) <- stepWire w1' ds mx'
                        m1 `seq` return (m1, w1 >-- w2)

main :: IO ()
main = do
    urza <- initUrza (0, 0) (400, 400) "quad-trees!"
    fontDir <- fmap (</> "Library" </> "Fonts") getHomeDirectory
    r       <- makeAsciiRenderer (fontDir </> "UbuntuMono-R.ttf") 8

    rColors <- randomColors
    let wini = def :: WindowIteration ()
        app  = newApp
        iter = wini { _iRender = renderAppWindow renderApp r
                    , _iWire   = windowWire appWire
                    , _iProcessEv = processInputEnv
                    , _iData = Right $ AppWindow { _awSize = Size 400 400
                                                 , _awBackgroundColor = Color.black
                                                 , _awApp = app rColors
                                                 }
                    }
    loopUrza urza iter


type AppTree = QTree (BoundingBox, Color4 Double)


randomColors :: IO [Color4 Double]
randomColors = do
    a <- newStdGen
    b <- newStdGen
    c <- newStdGen
    let rs = randomRs (0, 1) a
        gs = randomRs (0, 1) b
        bs = randomRs (0, 1) c
    return $ zipWith3 (\r g b' -> Color4 r g b' 1) rs gs bs


newQTree :: AppTree
newQTree = QT.empty $ Rectangle 0 0 400 400

newApp :: [Color4 Double] -> App
newApp colors = App { appInputRect = zeroRect
                    , appTree = newQTree
                    , appColors = colors
                    , appCollisions = []
                    }

windowWire :: (InputWire a a) -> InputWire (AppWindow a) (AppWindow a)
windowWire wire = appWire' <|> pass
    where appWire' = proc (AppWindow _ bg app) -> do
                         s'   <- windowSize -< ()
                         app' <- wire -< app
                         returnA -< AppWindow s' bg app'

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

