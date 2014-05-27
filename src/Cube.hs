module Cube where

import           Urza
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)

renderCube :: IO ()
renderCube = do
    i <- genObjectName
    bindBuffer ElementArrayBuffer $= Just i
    withArray ndxs $ \ptr -> do
        bufferData ElementArrayBuffer $= (sizeOfUByteList ndxs, ptr, StaticDraw)
    (_,_) <- bindAndBufferVerts3Colors vs cs
    return ()

vs :: [GLfloat]
vs = [ -0.5,  0.5, -0.5 -- Front face
     ,  0.5,  0.5, -0.5
     , -0.5, -0.5, -0.5
     ,  0.5, -0.5, -0.5
     , -0.5,  0.5,  0.5 -- Back face
     ,  0.5,  0.5,  0.5
     , -0.5, -0.5,  0.5
     ,  0.5, -0.5,  0.5
     ]

cs :: [GLfloat]
cs = [ 0, 1, 0, 1
     , 1, 1, 0, 1
     , 0, 0, 0, 1
     , 1, 0, 0, 1

     , 0, 1, 1, 1
     , 1, 1, 1, 1
     , 0, 0, 1, 1
     , 1, 0, 1, 1
     ]

ndxs :: [GLubyte]
ndxs = [ 0, 1, 2
       , 1, 2, 3

       , 4, 5, 6
       , 5, 6, 7

       , 4, 5, 0
       , 5, 0, 1

       , 6, 7, 2
       , 7, 2, 3

       , 1, 5, 3
       , 5, 3, 7

       , 4, 0, 6
       , 0, 6, 2
       ]

