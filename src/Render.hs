module Render where

import           Prelude hiding ((.), id, until)
import           Urza as U
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)
import           Linear hiding (trace)

renderViewport :: ShaderProgram -> V2 Int -> V2 Int -> Color4 GLfloat -> IO ()
renderViewport shdr (V2 vpw vph) (V2 w h) c = do
    viewport $= (Position 0 0, Size (fromIntegral vpw) (fromIntegral vph))
    clearColor $= c
    depthFunc $= Nothing --Just Lequal
    blend $= Enabled
    blendEquationSeparate $= (FuncAdd, FuncAdd)
    blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, Zero))
    clear [ColorBuffer, DepthBuffer]
    _setProjection shdr $ orthoM44 0 (fromIntegral w) 0 (fromIntegral h) 0 1
    _setModelview shdr $ eye4

