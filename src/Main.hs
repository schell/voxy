{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
module Main where

import           Prelude hiding ((.), id, until)
import           Urza
import           FRP.Netwire
import           Control.Monad.Reader hiding (when)
import qualified Urza.Color as Color
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)
import           Control.Lens hiding ((#), at)
import           System.Directory
import           System.FilePath ((</>))
import           Linear hiding (trace)
import           Debug.Trace


data Stage = Stage { _sWindowSize      :: Size
                   , _sBackgroundColor :: Color4 GLfloat
                   , _sFace            :: Bitmap_Transform2d
                   } deriving (Show)

type Render a = Renderer -> Either () a -> IO (Either () a)

renderFace :: Render Bitmap_Transform2d
renderFace _ (Left ()) = return $ Left ()
renderFace r (Right (bmp, tfrm)) = do
    drawBitmap r bmp tfrm
    return $ Right (bmp, tfrm)


renderStage :: Render Stage
renderStage _ (Left ()) = do
    clearColor $= Color.black
    clear [ColorBuffer, DepthBuffer]
    return $ Left ()
renderStage r (Right (Stage s@(Size w h) c face)) = do
    viewport $= (Position 0 0, s)
    clearColor $= c
    depthFunc $= Just Lequal
    blend $= Enabled
    blendEquationSeparate $= (FuncAdd, FuncAdd)
    blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, Zero))
    clear [ColorBuffer, DepthBuffer]
    r^.shader.setProjection $ orthoM44 0 (fromIntegral w) 0 (fromIntegral h) 0 1
    r^.shader.setModelview $ eye4
    r^.shader.setTextColor $ Color.yellow
    _ <- drawTextAt' r (Position 0 0) "Hey y'all."
    _ <- drawTextAt' r (Position 10 10) "Hey y'all."
    _ <- renderFace r $ Right face
    return $ Right $ Stage (Size w h) c face


stageWire :: Wire TimeDelta () (ReaderT InputEnv Identity) Stage Stage
stageWire = proc (Stage _ bc f) -> do
    s' <- windowSize -< ()
    f' <- myWire -< f
    returnA -< Stage s' bc f'


main :: IO ()
main = do
    urza <- initUrza (100, 100) (800, 600) "Urza"
    fontDir <- fmap (</> "Library" </> "Fonts") getHomeDirectory
    r       <- makeAsciiRenderer (fontDir </> "UbuntuMono-R.ttf") 24

    imgDir  <- fmap (</> "assets" </> "img") getCurrentDirectory
    Just bmp <- loadBitmap (imgDir </> "face.png")
    let face =  (bmp, def{ _t2Size = bmp^.bitmapSize})
        wini = def :: WindowIteration ()
        iter = wini { _iRender = renderStage r
                    , _iWire   = stageWire
                    , _iProcessEv = processInputEnv
                    , _iData = Right $ Stage { _sWindowSize = Size 800 600
                                             , _sBackgroundColor = Color.black
                                             , _sFace = face
                                             }
                    }
    loopUrza urza iter


myWire :: InputWire Bitmap_Transform2d Bitmap_Transform2d
myWire = proc (bmp, tfrm) -> do
    pos <- tweenPosWire -< ()
    returnA -< (bmp, tfrm & t2Position .~ pos)


textWire :: InputWire () String
textWire = ("Window size: " ++) . show <$> asSoonAs . windowResizeEvent


posWire :: InputWire () Position
posWire = cursor2Pos . asSoonAs . mouseButtonEvent MouseButton'1 MouseButtonState'Pressed


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
