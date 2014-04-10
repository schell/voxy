module Main where

import           Urza
import qualified Urza.Color as Color
import           Urza.Shader
import           Foreign
import           Control.Concurrent.MVar
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)
import           Control.Lens hiding ((#))
import           Control.Monad
import           System.Directory
import           System.FilePath ((</>))
import           System.Exit




main :: IO ()
main = do
    let txt = "Hey there!\nHello."

    mvar <- newMVar 0
    wvar <- initUrza (100,100) (800,600) "Voxy Town"
    fontDir <- fmap (</> "assets" </> "font") getCurrentDirectory
    imgDir  <- fmap (</> "assets" </> "img") getCurrentDirectory

    r <- loadText txt =<< makeRenderer (fontDir </> "Deutsch.ttf") 128
    Just (bmp@(Bitmap t size)) <- loadBitmap (imgDir </> "oryx_roguelike_16x24.png")

    let vs = [ -1,  1, -1 -- Front face
             ,  1,  1, -1
             , -1, -1, -1
             ,  1, -1, -1
             , -1,  1,  1 -- Back face
             ,  1,  1,  1
             , -1, -1,  1
             ,  1, -1,  1
             ]
        --cs = concat $ replicate 4 [ 1,1,1,1 ]
        cs = [ 0, 1, 0, 1
             , 1, 1, 0, 1
             , 0, 0, 0, 1
             , 1, 0, 0, 1

             , 0, 1, 1, 1
             , 1, 1, 1, 1
             , 0, 0, 1, 1
             , 1, 0, 1, 1
             ]
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
               ] :: [GLubyte]


        tn = translationMatrix3d 100 100 0
        s  = scaleMatrix3d 10 10 10
        rt = rotationMatrix3d 0 0 0
        mv = identityN 4 --`multiply` s --rt `multiply` s `multiply` tn

    r^.shader.setIs3d $ True
    r^.shader.setIsTextured $ False
    r^.shader.setColorIsReplaced $ False
    clientState VertexArray $= Enabled 

    --print vs
    i <- genObjectName
    bindBuffer ElementArrayBuffer $= Just i
    withArray ndxs $ \ptr -> do
        bufferData ElementArrayBuffer $= (sizeOfUByteList ndxs, ptr, StaticDraw)
    (j,k) <- bindAndBufferVerts3Colors vs cs


    forever $ do
        pollEvents
        (_, window) <- takeMVar wvar
        (winW, winH) <- fmap (over both fromIntegral) $ getWindowSize window

        makeContextCurrent $ Just window
        viewport $= (Position 0 0, Size winW winH)
        clearColor $= Color.black
        clear [ColorBuffer, DepthBuffer]

        r^.shader.setProjection $ concat $ orthoMatrix (-10) 10 10 (-10) (-10) 10
        rads <- takeMVar mvar
        let mv' = mv `multiply` rotationMatrix3d rads rads rads
        putMVar mvar $ rads + 0.01
        r^.shader.setModelview $ concat mv'
        drawElements Triangles (fromIntegral $ length ndxs) UnsignedByte nullPtr

        --drawElements Triangles 3 UnsignedInt nullPtr
        --drawArrays Triangles 0 6

        -- Render the display list.
--        modifyMVar_ sceneVar $ renderScene (Size winW winH)
        swapBuffers window
        shouldClose <- windowShouldClose window
        putMVar wvar ([],window)
        when shouldClose exitSuccess
