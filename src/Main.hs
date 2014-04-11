module Main where

import           Urza
import qualified Urza.Color as Color
import           Urza.Shader
import           Foreign hiding (void)
import           Control.Concurrent.MVar
import           Control.Concurrent
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)
import           Control.Lens hiding ((#))
import           Control.Monad
import           Data.Monoid
import           Data.Maybe
import qualified Data.Foldable as F
import           System.Directory
import           System.FilePath ((</>))
import           System.Exit
import           Debug.Trace
import           Linear hiding (trace)



type Camera = M44 GLfloat

data Input = Input { _iCursor :: (Double,Double)
                   , _iKeys   :: [Key]
                   } deriving (Show)


updateInput :: Input -> [InputEvent] -> Input
updateInput = foldl addEvent


addEvent :: Input -> InputEvent -> Input
addEvent i@(Input _ keys) (KeyEvent key _ KeyState'Pressed _) = i{_iKeys = key:keys}
addEvent i@(Input _ keys) (KeyEvent key _ KeyState'Released _) = i{_iKeys = filter (/=key) keys}
addEvent i@(Input _ _) (CursorMoveEvent x' y') = i{_iCursor = (x',y')}
addEvent i _ = i


updateCamera :: Camera -> Input -> Input -> Camera
updateCamera c i i' =
    let ts = 0.1
        rs = 1/180
        dx = realToFrac $ m'x - mx
        dy = realToFrac $ m'y - my
        (mx,my) = _iCursor i
        (m'x,m'y) = _iCursor i'
    in trace (show (dx,dy)) $
       translateCamera i' Key'W ts vOut      $
       translateCamera i' Key'A (-ts) vRight $
       translateCamera i' Key'S (-ts) vOut   $
       translateCamera i' Key'D ts vRight    $
       rotateCamera (dx,dy) rs c


translateCamera :: Input -> Key -> GLfloat -> (M44 GLfloat -> V3 GLfloat) -> Camera -> Camera
translateCamera i key speed vf c =
    if key `elem` _iKeys i
      then c !*! t
      else c
    where t = transM44 dx dy dz
          V3 dx dy dz = fmap (*speed) $ vf c


rotateCamera :: (GLfloat, GLfloat) -> GLfloat -> Camera -> Camera
rotateCamera (dx,dy) s c =
    if abs dx > 0 || abs dy > 0
      then c !*! r !*! u
      else c
    where r = mkTransformation rq $ V3 0 0 0
          rq = axisAngle (vRight c) $ s * dy
          u = mkTransformation uq $ V3 0 0 0
          uq = axisAngle (vUp c) $ s * dx


main :: IO ()
main = do
    let txt = map toEnum [32..126]

    camvar <- newMVar eye4
    invar  <- newMVar $ Input (0,0) []
    wvar <- initUrza (100,100) (800,600) "Voxy Town"
    fontDir <- fmap (</> "Library" </> "Fonts") getHomeDirectory
    imgDir  <- fmap (</> "assets" </> "img") getCurrentDirectory

    r <- loadText txt =<< makeRenderer (fontDir </> "UbuntuMono-R.ttf") 24
    Just (bmp@(Bitmap t size)) <- loadBitmap (imgDir </> "oryx_roguelike_16x24.png")

    let vs = [ -0.5,  0.5, -0.5 -- Front face
             ,  0.5,  0.5, -0.5
             , -0.5, -0.5, -0.5
             ,  0.5, -0.5, -0.5
             , -0.5,  0.5,  0.5 -- Back face
             ,  0.5,  0.5,  0.5
             , -0.5, -0.5,  0.5
             ,  0.5, -0.5,  0.5
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


    --print vs
    i <- genObjectName
    bindBuffer ElementArrayBuffer $= Just i
    withArray ndxs $ \ptr -> do
        bufferData ElementArrayBuffer $= (sizeOfUByteList ndxs, ptr, StaticDraw)
    (j,k) <- bindAndBufferVerts3Colors vs cs

    forever $ do
        pollEvents
        -- Do all of our state mutation up front right here.
        (events, window) <- takeMVar wvar
        input            <- takeMVar invar
        cam              <- takeMVar camvar
        let input' = updateInput input events
            cam'   = updateCamera cam input input'
        putMVar invar input'
        putMVar camvar $ cam'
        putMVar wvar ([],window)

        -- Every thing else is just about displaying our state.
        (winW, winH) <- fmap (over both fromIntegral) $ getWindowSize window

        let aspr = (fromIntegral winW / fromIntegral winH)

        makeContextCurrent $ Just window
        viewport $= (Position 0 0, Size winW winH)
        clearColor $= Color.black
        depthFunc $= Just Less
        clear [ColorBuffer, DepthBuffer]

        r^.shader.setIs3d $ True
        r^.shader.setIsTextured $ False
        r^.shader.setColorIsReplaced $ False
        clientState VertexArray $= Enabled
        r^.shader.setProjection $ perspectM44 45 aspr 0.1 10
        --r^.shader.setProjection $ toList $ orthoM44 (-10) 10 (-10) 10 (-10) 10
        r^.shader.setModelview $ cam' !*! mkTransformationMat eye3 (V3 0 0 (-5))
        drawElements Triangles (fromIntegral $ length ndxs) UnsignedByte nullPtr

        r^.shader.setProjection $ orthoM44 0 (fromIntegral winW) 0 (fromIntegral winH) 0 1
        r^.shader.setModelview $ eye4
        r^.shader.setTextColor $ Color4 1 1 0 1
        drawTextAt r (Position 0 0) "Weee!"

        -- Render the display list.
--        modifyMVar_ sceneVar $ renderScene (Size winW winH)
        swapBuffers window
        shouldClose <- windowShouldClose window
        when shouldClose exitSuccess
