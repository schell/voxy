module Render where

import           Prelude hiding ((.), id, until)
import           Urza as U
import           Control.Lens hiding ((#), at)
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)
import           Linear hiding (trace)

type Render a = a -> IO a

renderViewport :: ShaderProgram -> Int -> Int -> Color4 GLfloat -> IO ()
renderViewport shdr w h c = do
    viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
    clearColor $= c
    depthFunc $= Just Lequal
    blend $= Enabled
    blendEquationSeparate $= (FuncAdd, FuncAdd)
    blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, Zero))
    clear [ColorBuffer, DepthBuffer]
    shdr^.setProjection $ orthoM44 0 (fromIntegral w) 0 (fromIntegral h) 0 1
    shdr^.setModelview $ eye4

