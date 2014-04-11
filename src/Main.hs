module Main where

import           Urza
import qualified Urza.Color as Color
import           Urza.Shader
import           Foreign
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



type Camera = Matrix GLfloat

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
    let s = 0.1
        dx = realToFrac $ i'x - ix
        dy = realToFrac $ i'y - iy
        (ix,iy) = _iCursor i
        (i'x,i'y) = _iCursor i'
    in translateCamera i' Key'W s outVector      $
       translateCamera i' Key'A (-s) rightVector $
       translateCamera i' Key'S (-s) outVector   $
       translateCamera i' Key'D s rightVector    $
       rotateCamera (dx,dy) s c


translateCamera :: Input -> Key -> GLfloat -> (Matrix GLfloat -> Vector GLfloat) -> Camera -> Camera
translateCamera i key speed vf c =
    if key `elem` _iKeys i
      then c `multiply` t
      else c
    where t = translationMatrix3d dx dy dz
          [dx,dy,dz] = map (*speed) $ vf c


rotateCamera :: (GLfloat, GLfloat) -> GLfloat -> Camera -> Camera
rotateCamera (dx,dy) s c =
    if dx > 0 || dy > 0
      then c `multiply` trace (show u) u
      else c
    where r = rotationMatrix3d rx ry rz
          u = rotationMatrix3d ux uy uz
          [rx,ry,rz] = rightVector c
          [ux,uy,uz] = map ((s*dx)*) $ upVector c


main :: IO ()
main = do
    let txt = map toEnum [32..126]

    camvar <- newMVar $ identityN 4
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


    let toList :: M44 a -> [a]
        toList = F.foldl (\vs (V4 a b c d) -> vs ++ [a,b,c,d]) []


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
        blend $= Disabled 
        depthFunc $= Just Less 
        clear [ColorBuffer, DepthBuffer]

        r^.shader.setIs3d $ True
        r^.shader.setIsTextured $ False
        r^.shader.setColorIsReplaced $ False
        clientState VertexArray $= Enabled
        --r^.shader.setProjection $ toList $ perspectM44 (pi/4) aspr 3 7
        r^.shader.setProjection $ toList $ orthoM44 (-10) 10 (-10) 10 (-10) 10
        r^.shader.setModelview $ toList $ eye4 !*! mkTransformationMat eye3 (V3 0 0 (-5))
        drawElements Triangles (fromIntegral $ length ndxs) UnsignedByte nullPtr

        r^.shader.setProjection $ concat $ orthoMatrix 0 (fromIntegral winW) 0 (fromIntegral winH) 0 1
        r^.shader.setModelview $ concat $ identityN 4
        r^.shader.setTextColor $ Color.red
        --drawTextAt r (Position 0 0) $ camToStr cam

        -- Render the display list.
--        modifyMVar_ sceneVar $ renderScene (Size winW winH)
        swapBuffers window
        shouldClose <- windowShouldClose window
        when shouldClose exitSuccess
