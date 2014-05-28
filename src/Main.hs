{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Prelude hiding ((.), id)
import           Urza
import           Tween
import           FRP.Netwire
import           Control.Wire
import           Control.Wire.Core
import           Control.Wire.Unsafe.Event
import           Data.Maybe
import qualified Urza.Color as Color
import           Control.Concurrent.MVar
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)
import           Control.Lens hiding ((#))
import qualified Control.Monad as M
import           System.Directory
import           System.FilePath ((</>))
import           System.Exit
import           Linear hiding (trace)

main :: IO ()
main = do
    let txt = map toEnum [32..126]

    wvar    <- initUrza (100,100) (800,600) "Voxy Town"
    wirevar <- newMVar (clockSession_, mousePosWire)
    fontDir <- fmap (</> "Library" </> "Fonts") getHomeDirectory
    imgDir  <- fmap (</> "assets" </> "img") getCurrentDirectory

    r <- loadText txt =<< makeRenderer (fontDir </> "UbuntuMono-R.ttf") 24
    --Just graph <- graphFromFile (imgDir </> "oryx_roguelike_16x24.png")
    Just bmp <- loadBitmap (imgDir </> "photo.png")
    Just face <- loadBitmap (imgDir </> "face.png")

    M.forever $ do
        pollEvents
        -- Do all of our state mutation up front right here.
        (events, window) <- takeMVar wvar

        putMVar wvar ([],window)

        -- Every thing else is just about displaying our state.
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
        r^.shader.setTextColor $ Color4 1 1 0 1
        r^.shader.setColorIsReplaced $ True

        -- Netwire experiments.
        (session, wire) <- takeMVar wirevar
        (ds, session')  <- stepSession session
        (mx, wire')     <- stepWire wire ds $ Right events

        putMVar wirevar (session', wire')

        case mx of
            Left e -> putStrLn $ "Inhibited: " ++ show e
            Right pos@(Position x y) -> do
                let pos2 = Position (x+5) (y+5)
                M.void $ drawTextAt' r pos $ show pos
                M.void $ drawTextAt' r pos2 $ show pos2
        drawBitmap r face (Position 150 0) (face^.bitmapSize) (Scale 1 1) (Rotation 0)

        swapBuffers window
        shouldClose <- windowShouldClose window
        M.when shouldClose exitSuccess


-- Should inhibit until the mouse is clicked, then runs posWire - but
-- instead it inhibets until the mouse is clicked then runs posWire, then
-- inhibits immediately after.
mousePosWire :: (HasTime t s, Monad m, Fractional t) => Wire s () m [InputEvent] Position
mousePosWire = posWire . asSoonAs . mouseClick

-- Animate back and forth horizontally.
posWire :: (HasTime t s, Monad m, Fractional t) => Wire s () m a Position
posWire = Position <$> tween <*> 0
    where tween1 = for 1 . fmap round (easeInOutExpo (0 :: Double) 200 1)
          tween2 = for 1 . fmap round (easeInOutExpo (200 :: Double) 0 1)
          tween  = tween1 --> tween2 --> tween

-- Inhibits until producing a `Just` value.
whenIsJust :: (Monad m, Monoid e) => Wire s e m (Maybe a) a
whenIsJust = fromJust <$> when isJust

-- Inhibits until a mouse move.
cursorMove :: (Monad m, Monoid e) => Wire s e m [InputEvent] (Event InputEvent)
cursorMove = now . whenIsJust . arr getMouseMoveEvent

-- Inhibits until a mouse click.
mouseClick :: (Monad m, Monoid e) => Wire s e m [InputEvent] (Event InputEvent)
mouseClick = now . whenIsJust . arr getMouseUpEvent

