{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Prelude hiding ((.), id, until)
import           Urza as U
import           FRP.Netwire hiding (app)
import           Control.Wire.Unsafe.Event
import qualified Urza.Color as Color
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)
import           System.Directory
import           System.Random
import           System.FilePath ((</>))
import           Debug.Trace
import           QTree as QT
import           Types
import           Render

randomColors :: IO [Color4 Double]
randomColors = do
    a <- newStdGen
    b <- newStdGen
    c <- newStdGen
    let rs = randomRs (0, 1) a
        gs = randomRs (0, 1) b
        bs = randomRs (0, 1) c
    return $ zipWith3 (\r g b' -> Color4 r g b' 1) rs gs bs

main :: IO ()
main = do
    urza <- initUrza (100, 100) (800, 600) "Urza"
    fontDir <- fmap (</> "Library" </> "Fonts") getHomeDirectory
    r       <- makeAsciiRenderer (fontDir </> "UbuntuMono-R.ttf") 24

    rColors <- randomColors
    let wini = def :: WindowIteration ()
        app  = newApp
        iter = wini { _iRender = renderAppWindow renderApp r
                    , _iWire   = appWire drawOneRect
                    , _iProcessEv = processInputEnv
                    , _iData = Right $ AppWindow { _awSize = Size 800 600
                                                 , _awBackgroundColor = Color.black
                                                 , _awApp = app rColors
                                                 }
                    }
    loopUrza urza iter

newQTree :: QTree String
newQTree = insert (Rectangle 10 10 20 20) "10 10 20 20" $ QT.empty (Rectangle 0 0 750 550)

newApp :: [Color4 Double] -> App
newApp colors = App { appInputRect = zeroRect
                    , appBoxes = []
                    , appColors = colors
                    , appPos = (0,0)
                    }

appWire :: (InputWire a a) -> InputWire (AppWindow a) (AppWindow a)
appWire wire = appWire' <|> traceWith "app"
    where appWire' = proc (AppWindow _ bg app) -> do
                         s'   <- windowSize -< ()
                         app' <- wire -< app
                         returnA -< AppWindow s' bg app'

type MouseEvent = Event (Double, Double)

instance (Show a) => Show (Event a) where
    show (Event a) = "Event (" ++ show a ++ ")"
    show NoEvent = "NoEvent"

getOneBox :: InputWire () BoundingBox
getOneBox = proc _ -> do
    (x,y)   <- asSoonAs . mouseButtonEvent MouseButton'1 MouseButtonState'Pressed -< ()
    (dx,dy) <- asSoonAs . cursorMoveEvent . arr (const ()) <|> pass -< (x,y)
    mouseup <- mouseButtonEvent MouseButton'1 MouseButtonState'Released -< ()
    until   -< (Rectangle x y (dx - x) (dy - y), mouseup)

--firstTillSecond :: (Wire s e m a b, Wire s e m a b) -> Wire s e m a b
--firstTillSecond (w1, w2) = proc a -> do

maybeWire :: (Monad m, Monoid e) => Wire s e m a (Maybe a)
maybeWire = (arr $ \a -> Just a) <|> pure Nothing


drawOneRect :: InputWire App App
drawOneRect = proc app -> do
    r <- traceWire . getOneBox -< ()
    returnA -< app { appInputRect = r }

pass :: Monad m => Wire s e m a a
pass = arr id

mouseDownLeft :: InputWire () (Event (Double, Double))
mouseDownLeft = mouseButtonEvent MouseButton'1 MouseButtonState'Pressed

mouseUpLeft :: InputWire () (Event (Double, Double))
mouseUpLeft = mouseButtonEvent MouseButton'1 MouseButtonState'Released

-- Animate back and forth horizontally.
tweenPosWire :: InputWire () Position
tweenPosWire = Position <$> tween <*> tween
    where tween1 = for 1 . fmap round (easeInOutExpo (0 :: Double) 200 1)
          tween2 = for 1 . fmap round (easeInOutExpo (200 :: Double) 0 1)
          tween  = tween1 --> tween2 --> tween

windowSize :: InputWire a Size
windowSize = (arr $ \(w, h) -> Size (fromIntegral w) (fromIntegral h)) . asSoonAs . windowResizeEvent

traceWire :: (Monad m, Show a) => Wire s e m a a
traceWire = arr (\a -> trace (show a) a)

traceWith :: (Monad m) => String -> Wire s e m a a
traceWith s = arr (\a -> trace s a)


