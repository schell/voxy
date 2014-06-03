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


-- | TODO: Get rid of the many MVars.

main :: IO ()
main = do
    wvar    <- initUrza (100,100) (800,600) "Wirez"
    fontDir <- fmap (</> "Library" </> "Fonts") getHomeDirectory
    imgDir  <- fmap (</> "assets" </> "img") getCurrentDirectory
    r       <- makeAsciiRenderer (fontDir </> "UbuntuMono-R.ttf") 24


    let renderFace :: Either () Bitmap_Transform2d -> IO (Either () Bitmap_Transform2d)
        renderFace (Left ()) = do
            Just face <- loadBitmap (imgDir </> "face.png")
            return $ Right (face, def{ _t2Size = trace (show $ face^.bitmapSize) face^.bitmapSize})
        renderFace (Right (bmp, tfrm)) = do
            drawBitmap r bmp tfrm
            return $ Right (bmp, tfrm)

    ivar <- newMVar $ def { _iRender = renderFace
                          , _iSession = clockSession_
                          , _iWire = myWire
                          }

    M.forever $ do
        -- Execute Urza callbacks and load up events.
        pollEvents
        -- Pop off the oldest event for processing.
        (events, window) <- takeMVar wvar
        let (mEvent, events') = if null events
                                  then (Nothing, [])
                                  else (Just $ last events, take (length events -1) events)
        -- Put the rest back for later.
        putMVar wvar (events', window)

        -- Pre render setup
        (winW, winH) <- fmap (over both fromIntegral) $ getWindowSize window
        makeContextCurrent $ Just window
        viewport $= (Position 0 0, Size winW winH)
        clearColor $= Color.black
        depthFunc $= Just Lequal
        blend $= Enabled
        blendEquationSeparate $= (FuncAdd, FuncAdd)
        blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, Zero))
        clear [ColorBuffer, DepthBuffer]

        r^.shader.setProjection $ orthoM44 0 (fromIntegral winW) 0 (fromIntegral winH) 0 1
        r^.shader.setModelview $ eye4

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
