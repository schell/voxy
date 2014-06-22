module Render where

import           Types
import           Prelude hiding ((.), id, until)
import           Urza as U
import           Control.Lens hiding ((#), at)
import qualified Urza.Color as Color
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)
import           Linear hiding (trace)

renderBitmap :: Renderer -> Bitmap -> IO ()
renderBitmap r bmp = drawTexture (r^.shader) (bmp^.bitmapTexture) $ def { _t2Size = size }
    where (w, h) = _bitmapSize bmp
          size = (fromIntegral w, fromIntegral h)

renderAppWindow :: ShaderProgram -> (a -> IO a) -> Render (AppWindow a)
renderAppWindow shdr _ (Left ()) = do
    clearColor $= Color.red
    clear [ColorBuffer, DepthBuffer]
    shdr^.setProjection $ orthoM44 0 100 0 100 0 1
    shdr^.setModelview $ eye4
    return $ Left ()
renderAppWindow shdr rndrApp (Right (AppWindow s@(Size w h) c app)) = do
    viewport $= (Position 0 0, s)
    clearColor $= c
    depthFunc $= Just Lequal
    blend $= Enabled
    blendEquationSeparate $= (FuncAdd, FuncAdd)
    blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, Zero))
    clear [ColorBuffer, DepthBuffer]
    shdr^.setProjection $ orthoM44 0 (fromIntegral w) 0 (fromIntegral h) 0 1
    shdr^.setModelview $ eye4
    app' <- rndrApp app
    return $ Right $ AppWindow (Size w h) c app'

