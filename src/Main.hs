{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
module Main where

import           Prelude hiding ((.), id, until)
import           Urza as U
import           FRP.Netwire hiding (app)
import           Control.Lens hiding ((#), at)
import           Control.Monad.Reader hiding (when)
import qualified Control.Monad as M
import qualified Urza.Color as Color
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)
import           System.Directory
import           System.Random
import           System.FilePath ((</>))
import           Linear hiding (trace)
import           Data.List hiding (insert)
import           Data.Maybe
import           Debug.Trace
import           QTree as QT


newtype ColorButton = ColorButton (Color4 Double) deriving (Eq)

newtype SizeButton = SizeButton Int deriving (Eq)

data Stage = Stage { _sColors :: [ColorButton]
                   , _sSizes  :: [SizeButton]
                   }


stage :: Stage
stage = Stage { _sColors = [ ColorButton Color.black
                           , ColorButton Color.white
                           , ColorButton Color.red
                           , ColorButton Color.green
                           , ColorButton Color.blue
                           , ColorButton Color.yellow
                           ]
              , _sSizes  = [ SizeButton 1
                           , SizeButton 2
                           , SizeButton 4
                           , SizeButton 8
                           , SizeButton 16
                           ]
              }

randomColors :: IO [Color4 Double]
randomColors = do
    a <- newStdGen
    b <- newStdGen
    c <- newStdGen
    let rs = randomRs (0, 1) a
        gs = randomRs (0, 1) b
        bs = randomRs (0, 1) c
    return $ zipWith3 (\r g b -> Color4 r g b 1) rs gs bs

renderStage :: Renderer -> Stage -> IO ()
renderStage r (Stage cs ss) = do
    let ndx :: (Eq a) => a -> [a] -> Maybe Double
        ndx x xs = fmap ((1+) . fromIntegral) $ elemIndex x xs
    _ <- M.forM cs $ \cb@(ColorButton c) -> do
        let Just i = ndx cb cs
        fillPath_ r $ do
            setColor $ Color4 0.5 0.5 0.5 1
            rectangleAt (i*30 -2) 8  24 24
        fillPath r $ do
            setColor c
            rectangleAt (i*30) 10 20 20
    _ <- M.forM ss $ \sb@(SizeButton s) -> do
        let Just i = ndx sb ss
        fillPath_ r $ do
            setColor Color.white
            circleOfNAt (190 + i*30) 20 12 16
        r^.shader.setTextColor $ Color.black
        drawTextAt' r (Position (180 + (round i)*30) 0) $ show s
    return ()


renderDrawingRects :: Renderer -> DrawingRects -> IO ()
renderDrawingRects r (DrawingRects cur bs _ _) = do
    M.when (isJust cur) $ fillPath_ r $ do
        setColor $ Color.white
        uncurryRectangle rectangleAt $ fromJust cur
    forM_ (reverse bs) $ \(c, bb) -> fillPath_ r $ do
        setColor c
        uncurryRectangle rectangleAt bb


data AppWindow a = AppWindow { _awSize            :: Size
                             , _awBackgroundColor :: Color4 GLfloat
                             , _awApp             :: a
                             }

data DrawingRects = DrawingRects { drCurrent :: Maybe BoundingBox
                                 , drBoxes   :: [(Color4 Double, BoundingBox)]
                                 , drColors  :: [Color4 Double]
                                 , drPos     :: (Double, Double)
                                 } deriving (Show)

type Render a = Renderer -> Either () a -> IO (Either () a)

renderFace :: Render Bitmap_Transform2d
renderFace _ (Left ()) = return $ Left ()
renderFace r (Right (bmp, tfrm)) = do
    drawBitmap r bmp tfrm
    return $ Right (bmp, tfrm)

renderQTree :: Renderer -> QTree String -> IO ()
renderQTree r (QTree bb mbs ls) = do
    fillPath_ r $ do
        setColor $ Color4 1 1 1 0.3
        uncurryRectangle rectangleAt bb
    strokePath_ r $ do
        setColor $ Color4 1 1 1 0.6
        uncurryRectangle rectangleAt bb
    forM_ ls $ \(QLeaf b e) -> do
        fillPath_ r $ do
            setColor $ Color4 1 1 0 0.3
            uncurryRectangle rectangleAt b
        strokePath_ r $ do
            setColor $ Color4 1 1 0 0.6
            uncurryRectangle rectangleAt b
        r^.shader.setTextColor $ Color4 0 0 0 0.8
        drawTextAt' r (Position (round $ U.left b) (round $ U.top b)) e
    case mbs of
        Nothing -> return ()
        Just (a,b,c,d) -> do
            forM_ [a,b,c,d] $ renderQTree r



renderAppWindow :: (Renderer -> a -> IO ()) -> Render (AppWindow a)
renderAppWindow _ r (Left ()) = do
    clearColor $= Color.black
    clear [ColorBuffer, DepthBuffer]
    r^.shader.setProjection $ orthoM44 0 100 0 100 0 1
    r^.shader.setModelview $ eye4
    r^.shader.setTextColor $ Color.red
    drawTextAt' r (Position 0 0) "Inhibited"
    return $ Left ()
renderAppWindow rndrApp r (Right (AppWindow s@(Size w h) c app)) = do
    viewport $= (Position 0 0, s)
    clearColor $= c
    depthFunc $= Just Lequal
    blend $= Enabled
    blendEquationSeparate $= (FuncAdd, FuncAdd)
    blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, Zero))
    clear [ColorBuffer, DepthBuffer]
    r^.shader.setProjection $ orthoM44 0 (fromIntegral w) 0 (fromIntegral h) 0 1
    r^.shader.setModelview $ eye4
    rndrApp r app
    return $ Right $ AppWindow (Size w h) c app


main :: IO ()
main = do
    urza <- initUrza (100, 100) (800, 600) "Urza"
    fontDir <- fmap (</> "Library" </> "Fonts") getHomeDirectory
    r       <- makeAsciiRenderer (fontDir </> "UbuntuMono-R.ttf") 24

    imgDir  <- fmap (</> "assets" </> "img") getCurrentDirectory
    Just bmp <- loadBitmap (imgDir </> "face.png")
    rColors <- randomColors
    let face =  (bmp, def{ _t2Size = bmp^.bitmapSize})
        wini = def :: WindowIteration ()
        drbx = newDrawingRects
        iter = wini { _iRender = renderAppWindow renderDrawingRects r
                    , _iWire   = appWire drWire
                    , _iProcessEv = processInputEnv
                    , _iData = Right $ AppWindow { _awSize = Size 800 600
                                                 , _awBackgroundColor = Color.black
                                                 , _awApp = drbx rColors
                                                 }
                    }
    loopUrza urza iter

newQTree :: QTree String
newQTree = insert (Rectangle 10 10 20 20) "10 10 20 20" $ QT.empty (Rectangle 0 0 750 550)

newDrawingRects :: [Color4 Double] -> DrawingRects
newDrawingRects colors = DrawingRects { drCurrent = Nothing
                                      , drBoxes = []
                                      , drPos = (0,0)
                                      , drColors = colors
                                      }

appWire :: (InputWire a a) -> InputWire (AppWindow a) (AppWindow a)
appWire wire = appWire' <|> passThru
    where appWire' = proc (AppWindow _ bg app) -> do
                         s'   <- windowSize -< ()
                         app' <- wire -< app
                         returnA -< AppWindow s' bg app'

drWire :: InputWire DrawingRects DrawingRects
drWire = traceWith "clicked" . placePoint <|>
         traceWith "reset"   . reset      <|>
         passThru
    where placePoint = proc (DrawingRects _ bxs (c:cs) _) -> do
                           (x,y) <- useNow . leftClicked -< ()
                           let r = (c, Rectangle (x-50) (y-50) 100 100)
                           returnA -< DrawingRects Nothing (r:bxs) cs (0,0)
          reset      = proc (DrawingRects _ _ cs _) -> do
                           useNow_ . onCharEvent 'r' -< ()
                           returnA -< DrawingRects Nothing [] cs (0,0)


passThru :: Monad m => Wire s e m a a
passThru = arr id


myWire :: InputWire Bitmap_Transform2d Bitmap_Transform2d
myWire = proc (bmp, tfrm) -> do
    pos <- tweenPosWire -< ()
    returnA -< (bmp, tfrm & t2Position .~ pos)

textWire :: InputWire () String
textWire = ("Window size: " ++) . show <$> asSoonAs . windowResizeEvent

posWire :: InputWire () Position
posWire = cursor2Pos . asSoonAs . mouseButtonEvent MouseButton'1 MouseButtonState'Pressed

leftClicked :: InputWire () (Event (Double, Double))
leftClicked = mouseButtonEvent MouseButton'1 MouseButtonState'Released

-- Animate back and forth horizontally.
tweenPosWire :: InputWire () Position
tweenPosWire = Position <$> tween <*> tween
    where tween1 = for 1 . fmap round (easeInOutExpo (0 :: Double) 200 1)
          tween2 = for 1 . fmap round (easeInOutExpo (200 :: Double) 0 1)
          tween  = tween1 --> tween2 --> tween

cursor2Pos :: InputWire (Double, Double) Position
cursor2Pos = arr $ \(x, y) -> Position (round x) (round y)

windowSize :: InputWire a Size
windowSize = (arr $ \(w, h) -> Size (fromIntegral w) (fromIntegral h)) . asSoonAs . windowResizeEvent

traceWire :: (Monad m, Show a) => Wire s e m a a
traceWire = arr (\a -> trace (show a) a)

traceWith :: (Monad m) => String -> Wire s e m a a
traceWith s = arr (\a -> trace s a)


stageWire :: Wire TimeDelta () (ReaderT InputEnv Identity) (AppWindow Stage) (AppWindow Stage)
stageWire = proc (AppWindow _ bc f) -> do
    s' <- windowSize -< ()
    returnA -< AppWindow s' bc f



