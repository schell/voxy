{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Prelude hiding ((.), id)
import           Urza
import           Tween
import           FRP.Netwire
import           Control.Wire
import           Control.Wire.Core
import           Control.Wire.Unsafe.Event
import           Control.Monad.Reader hiding (when)
import           Data.Maybe
import qualified Data.Set as S
import qualified Urza.Color as Color
import           Control.Concurrent.MVar
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)
import           Control.Lens hiding ((#), at)
import qualified Control.Monad as M
import           System.Directory
import           System.FilePath ((</>))
import           System.Exit
import           Linear hiding (trace)

type Env = Maybe InputEvent

type Step s w b = (s, w, b)

type AppWire s b = Wire s () (ReaderT Env IO) () b

-- | TODO: Get rid of the many MVars.

main :: IO ()
main = do
    let txt = map toEnum [32..126]

    wvar    <- initUrza (100,100) (800,600) "Voxy Town"
    wirevar <- newMVar (clockSession_, myWire, Right $ Position 0 0)
    fontDir <- fmap (</> "Library" </> "Fonts") getHomeDirectory
    imgDir  <- fmap (</> "assets" </> "img") getCurrentDirectory

    r <- loadText txt =<< makeRenderer (fontDir </> "UbuntuMono-R.ttf") 24
    --Just graph <- graphFromFile (imgDir </> "oryx_roguelike_16x24.png")
    Just bmp <- loadBitmap (imgDir </> "photo.png")
    Just face <- loadBitmap (imgDir </> "face.png")

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

        (session, wire, _) <- takeMVar wirevar
        (session', wire', mx) <- case mEvent of
            Nothing -> stepTime session wire
            Just ev -> do (session', wire', _) <- stepEvent session wire ev
                          stepTime session' wire'
        putMVar wirevar (session', wire', mx)

        -- Render!
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

        case mx of
            Left e -> putStrLn $ "Inhibited"
            Right pos@(Position x y) -> do
                let pos2 = Position (x+5) (y+5)
                M.void $ drawTextAt' r pos $ show pos
                M.void $ drawTextAt' r pos2 $ show pos2
        drawBitmap r face (Position 150 0) (face^.bitmapSize) (Scale 1 1) (Rotation 0)

        swapBuffers window
        shouldClose <- windowShouldClose window
        M.when shouldClose exitSuccess

stepTime :: Session IO s -> AppWire s b -> IO (Session IO s, AppWire s b, Either () b)
stepTime session wire = do
    (ds, session')  <- stepSession session
    (mx, wire') <- runReaderT (stepWire wire ds $ Right ()) Nothing
    return (session', wire', mx)

stepEvent :: (Monoid s) => t -> AppWire s b -> InputEvent -> IO (t, AppWire s b, Either () b)
stepEvent session wire ev = do
    print ev
    (mx, wire') <- runReaderT (stepWire wire mempty $ Right ()) $ Just ev
    return (session, wire', mx)

keyEvent :: MonadReader Env m => Key -> KeyState -> Wire s e m a (Event a)
keyEvent key kstate = mkGen_ $ \a -> do
    mEv <- ask
    return $ Right $ case mEv of
        Just (KeyEvent k _ ks _) -> if k == key && ks == kstate then Event a else NoEvent
        _ -> NoEvent

cursorMoveEvent :: MonadReader Env m => Wire s e m a (Event (Double, Double))
cursorMoveEvent = mkGen_ $ \_ -> do
    mEv <- ask
    return $ Right $ case mEv of
        Just (CursorMoveEvent x y) -> Event (x, y)
        _ -> NoEvent

cursorEnterEvent :: MonadReader Env m => CursorState -> Wire s e m a (Event a)
cursorEnterEvent cState = mkGen_ $ \a -> do
    mEv <- ask
    return $ Right $ case mEv of
        Just (CursorEnterEvent s) -> if cState == s then Event a else NoEvent
        _ -> NoEvent


myWire :: (MonadReader (Maybe InputEvent) m, HasTime t s, Monoid e, Fractional t) => Wire s e m a Position
myWire = cursor2Pos . asSoonAs . cursorMoveEvent <|> (pure $ Position 0 0)
    where cursor2Pos = arr $ \(x, y) -> Position (round x) (round y)

-- Animate back and forth horizontally.
posWire :: (Monad m, Monoid e, HasTime t s, Fractional t) => Wire s e m a Position
posWire = Position <$> tween <*> 0
    where tween1 = for 1 . fmap round (easeInOutExpo (0 :: Double) 200 1)
          tween2 = for 1 . fmap round (easeInOutExpo (200 :: Double) 0 1)
          tween  = tween1 --> tween2 --> tween

