{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Prelude hiding ((.), id, until)
import           Urza
import           Urza.Wire
import           Tween
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


-- | TODO: Get rid of the many MVars.

main :: IO ()
main = do
    let txt = map toEnum [32..126]

    wvar    <- initUrza (100,100) (800,600) "Wirez"
    evar    <- newMVar def 
    wirevar <- newMVar (clockSession_, textAndPosWire, Right ("...", Position 0 0))
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

        -- Process, update and return our environment.
        env <- processEnv mEvent <$> takeMVar evar
        putMVar evar env

        (session, wire, _) <- takeMVar wirevar
        (session', wire', mx) <- step session wire env
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
            Left _ -> putStrLn $ "Inhibited"
            Right (text, pos) -> do
                M.void $ drawTextAt' r pos text
        drawBitmap r face (Position 0 50) (face^.bitmapSize) (Scale 1 1) (Rotation 0)

        swapBuffers window
        shouldClose <- windowShouldClose window
        M.when shouldClose exitSuccess

--myWire :: (MonadReader Env m, HasTime t s, Monoid e, Fractional t) => Wire s e m a String
--myWire = whenCursorIsOnScreen . cursorPositionStartingWith (Position 0 0) <|> pure (Position 0 0)

data Transform2d = Transform2d { _t2Position :: Position
                               , _t2Size     :: Size
                               , _t2Scale    :: Scale
                               , _t2Rotation :: Rotation
                               } deriving (Show)

data Renderable2d a = Renderable2d { _rTransform :: Transform2d
                                   , _rData      :: a
                                   , _rRender    :: a -> IO ()
                                   }

textAndPosWire :: (MonadReader Env m, HasTime t s, Monoid e, Fractional t) => Wire s e m a (String, Position)
textAndPosWire = (,) <$> stringWire "" <*> posWire 

textWire :: (MonadReader Env m, HasTime t s, Monoid e, Fractional t) => Wire s e m a String
textWire = ("Window size: " ++) . show <$> asSoonAs . windowResizeEvent 

stringWire :: (MonadReader Env m, HasTime t s, Monoid e, Fractional t) => String -> Wire s e m a String
stringWire str = asSoonAs . accumE (\b a -> b ++ [a]) str . charEvent

posWire :: (MonadReader Env m, HasTime t s, Monoid e, Fractional t) => Wire s e m a Position
posWire = cursor2Pos . asSoonAs . mouseButtonEvent MouseButton'1 MouseButtonState'Pressed

-- Animate back and forth horizontally.
tweenPosWire :: (Monad m, Monoid e, HasTime t s, Fractional t) => Wire s e m a Position
tweenPosWire = Position <$> tween <*> tween
    where tween1 = for 1 . fmap round (easeInOutExpo (0 :: Double) 200 1)
          tween2 = for 1 . fmap round (easeInOutExpo (200 :: Double) 0 1)
          tween  = tween1 --> tween2 --> tween

cursor2Pos :: (Monad m) => Wire s e m (Double, Double) Position
cursor2Pos = arr $ \(x, y) -> Position (round x) (round y)
