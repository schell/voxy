{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
module Main where

import           Prelude hiding ((.), id, until)
import           Urza
import           FRP.Netwire
import           Control.Monad.Reader hiding (when)
import qualified Urza.Color as Color
import           Control.Concurrent.MVar
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)
import           Control.Lens hiding ((#), at)
import qualified Control.Monad as M
import           System.Directory
import           System.FilePath ((</>))
import           System.Exit
import           Linear hiding (trace)
import           Debug.Trace
import           Data.Maybe


data Stage = Stage { _sWindowSize      :: Size
                   , _sBackgroundColor :: Color4 GLfloat
                   } deriving (Show)

instance Default Stage where
    def = Stage { _sWindowSize = Size 800 600
                , _sBackgroundColor = Color.black
                }

type Render a = Renderer -> Either () a -> IO (Either () a)

renderFace :: Render Bitmap_Transform2d
renderFace _ (Left ()) = do
    imgDir  <- fmap (</> "assets" </> "img") getCurrentDirectory
    Just face <- loadBitmap (imgDir </> "face.png")
    return $ Right (face, def{ _t2Size = trace (show $ face^.bitmapSize) face^.bitmapSize})
renderFace r (Right (bmp, tfrm)) = do
    drawBitmap r bmp tfrm
    return $ Right (bmp, tfrm)


renderStage :: Render Stage
renderStage _ (Left ()) = return $ Right def
renderStage r (Right (Stage s@(Size w h) c)) = do
    viewport $= (Position 0 0, s)
    clearColor $= c
    depthFunc $= Just Lequal
    blend $= Enabled
    blendEquationSeparate $= (FuncAdd, FuncAdd)
    blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, Zero))
    clear [ColorBuffer, DepthBuffer]
    r^.shader.setProjection $ orthoM44 0 (fromIntegral w) 0 (fromIntegral h) 0 1
    r^.shader.setModelview $ eye4
    r^.shader.setTextColor $ Color.black
    _ <- drawTextAt' r (Position 0 0) "Hey y'all."
    return $ Right $ Stage (Size w h) c


main :: IO ()
main = do
    wvar    <- initUrza (100,100) (800,600) "Wirez"
    fontDir <- fmap (</> "Library" </> "Fonts") getHomeDirectory
    r       <- makeAsciiRenderer (fontDir </> "UbuntuMono-R.ttf") 24

    ivar <- newMVar $ def { _iRender = renderStage r
                          , _iWire = Stage <$> windowSize <*> pure Color.red <|> pure def
                          }

    M.forever $ do
        -- Execute Urza callbacks and load up events.
        pollEvents
        -- Pop off the oldest event for processing.
        (events, window) <- takeMVar wvar
        let (mEvent, events') = if null events
                                  then (Nothing, [])
                                  else (Just $ head events, drop 1 events)
        M.when (isJust mEvent) $ putStrLn $ show mEvent
        -- Put the rest back for later.
        putMVar wvar (events', window)

        -- Pre render setup
        makeContextCurrent $ Just window


        -- Process, update and render our app iteration.
        stepAndRender ivar mEvent

        swapBuffers window
        shouldClose <- windowShouldClose window
        M.when shouldClose exitSuccess

myWire :: (MonadReader Env m, Monoid e) => Wire TimeDelta e m Bitmap_Transform2d Bitmap_Transform2d
myWire = proc (bmp, tfrm) -> do
    pos <- tweenPosWire -< ()
    returnA -< (bmp, tfrm & t2Position .~ pos)


textAndPosWire :: (MonadReader Env m, Monoid e) => Wire TimeDelta e m a (String, Position)
textAndPosWire = (,) <$> stringWire "" <*> posWire

textWire :: (MonadReader Env m, Monoid e) => Wire TimeDelta e m a String
textWire = ("Window size: " ++) . show <$> asSoonAs . windowResizeEvent

stringWire :: (MonadReader Env m, Monoid e) => String -> Wire TimeDelta e m a String
stringWire str = asSoonAs . accumE (\b a -> b ++ [a]) str . charEvent


posWire :: (MonadReader Env m, Monoid e) => Wire TimeDelta e m a Position
posWire = cursor2Pos . asSoonAs . mouseButtonEvent MouseButton'1 MouseButtonState'Pressed


-- Animate back and forth horizontally.
tweenPosWire :: (Monad m, Monoid e) => Wire TimeDelta e m a Position
tweenPosWire = Position <$> tween <*> tween
    where tween1 = for 1 . fmap round (easeInOutExpo (0 :: Double) 200 1)
          tween2 = for 1 . fmap round (easeInOutExpo (200 :: Double) 0 1)
          tween  = tween1 --> tween2 --> tween


cursor2Pos :: (Monad m) => Wire TimeDelta e m (Double, Double) Position
cursor2Pos = arr $ \(x, y) -> Position (round x) (round y)


windowSize :: Wire s () (ReaderT Env Identity) a Size
windowSize = traceWire . (arr $ \(w, h) -> Size (fromIntegral w) (fromIntegral h)) . asSoonAs . windowResizeEvent

traceWire :: (Monad m, Show a) => Wire s e m a a
traceWire = arr (\a -> trace (show a) a)
