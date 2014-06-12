module Render where

import           Types
import           Prelude hiding ((.), id, until)
import           Urza as U
import           FRP.Netwire hiding (app)
import           Control.Lens hiding ((#), at)
import           Control.Monad.Reader hiding (when)
import qualified Control.Monad as M
import qualified Urza.Color as Color
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)
import           Linear hiding (trace)
import           Data.List hiding (insert)
import           QTree as QT


renderAppWindow :: (Renderer -> a -> IO ()) -> Render (AppWindow a)
renderAppWindow _ r (Left ()) = do
    clearColor $= Color.black
    clear [ColorBuffer, DepthBuffer]
    r^.shader.setProjection $ orthoM44 0 100 0 100 0 1
    r^.shader.setModelview $ eye4
    r^.shader.setTextColor $ Color.red
    _ <- drawTextAt' r (Position 0 0) "Inhibited"
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


renderApp :: Renderer -> App -> IO ()
renderApp r (App cur bs _ (x,y)) = do
    putStr $ if x > y then "" else ""
    M.when (areaOf cur > 0) $ fillPath_ r $ do
        setColor $ Color.white
        uncurryRectangle rectangleAt cur
    forM_ (reverse bs) $ \(c, bb) -> fillPath_ r $ do
        setColor c
        uncurryRectangle rectangleAt bb


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

