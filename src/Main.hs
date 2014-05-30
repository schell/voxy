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

type Env = S.Set Key

type AppWire s b = Wire s () (ReaderT Env IO) () b

-- | TODO: Get rid of the many MVars.

main :: IO ()
main = do
    let txt = map toEnum [32..126]

    wvar    <- initUrza (100,100) (800,600) "Voxy Town"
    envvar  <- newMVar S.empty
    wirevar <- newMVar (clockSession_, keyPosWire)
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

        -- mx is our time/event varying value.
        mx <- case mEvent of
            Nothing -> stepTime wirevar envvar
            Just ev -> stepEvent ev wirevar envvar

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
            Left e -> putStrLn $ "Inhibited: " ++ show e
            Right pos@(Position x y) -> do
                let pos2 = Position (x+5) (y+5)
                M.void $ drawTextAt' r pos $ show pos
                M.void $ drawTextAt' r pos2 $ show pos2
        drawBitmap r face (Position 150 0) (face^.bitmapSize) (Scale 1 1) (Rotation 0)

        swapBuffers window
        shouldClose <- windowShouldClose window
        M.when shouldClose exitSuccess

stepTime :: MVar (Session IO s, AppWire s b) -> MVar Env -> IO (Either () b)
stepTime wirevar envvar = do
    env <- readMVar envvar
    (session, wire) <- takeMVar wirevar
    (ds, session')  <- stepSession session
    (mx, wire') <- runReaderT (stepWire wire ds $ Right ()) env
    putMVar wirevar (session', wire')
    return mx

stepEvent :: (Monoid s) => InputEvent -> MVar (Session IO s, AppWire s b) -> MVar Env -> IO (Either () b)
stepEvent (KeyEvent key _ kstate _) wirevar envvar = do
    putStrLn $ show key ++ " " ++ show kstate
    env <- takeMVar envvar
    (session, wire) <- takeMVar wirevar
    let env' = if kstate == KeyState'Pressed
                 then S.delete key env
                 else S.insert key env
    (mx, wire') <- runReaderT (stepWire wire mempty $ Right ()) env'
    putMVar wirevar (session, wire')
    putMVar envvar env'
    return mx
stepEvent _ wirevar envvar = do
    env <- readMVar envvar
    (session, wire) <- takeMVar wirevar
    (ds, session')  <- stepSession session
    (mx, wire') <- runReaderT (stepWire wire ds $ Right ()) env
    putMVar wirevar (session', wire')
    return mx

keyIsDown :: (MonadReader (S.Set a) m, Ord a) => a -> Wire s e m a1 (Event a1)
keyIsDown key =
    mkGen_ $ \x -> do
        isPressed <- asks (S.member key)
        return $ if isPressed then Right (Event x) else Right NoEvent

keyPosWire = asSoonAs . keyIsDown Key'A . (pure $ Position 500 500) <|> posWire

-- Animate back and forth horizontally.
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

-- For testing events.
addInputEventsEvery :: (HasTime t s, Monad m, Monoid e) => t -> Wire s e m a (Event [InputEvent])
addInputEventsEvery t = after 2 . periodic t . pure [CursorMoveEvent 0 0]
