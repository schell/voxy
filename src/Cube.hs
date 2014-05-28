module Cube where

import           Urza
import           Linear
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)

data Camera = Camera { _camXYZ :: (Double, Double, Double) -- ^ x y z position
                     , _camRot :: (Double, Double, Double) -- ^ pitch yaw roll
                     } deriving (Show, Eq)


identityCamera :: Camera
identityCamera = Camera (0,0,0) (0,0,0)


toMatrix :: Camera -> M44 GLfloat
toMatrix (Camera (x,y,z) (rx, ry, _)) = rot !*! tfrm
    where [x', y', z', rx', ry'] = map realToFrac [x,y,z,rx,ry]
          qp  = axisAngle (V3 1 0 0) ry'
          qy  = axisAngle (V3 0 1 0) rx'
          rot = mkTransformationMat (fromQuaternion qp !*! fromQuaternion qy) $ V3 0 0 0
          tfrm = mkTransformationMat eye3 $ V3 x' y' z'


data Input = Input { _iCursor :: (Double,Double)
                   , _iKeys   :: [Key]
                   , _iInitialized :: Bool
                   } deriving (Show)

    --camvar  <- newMVar $ Camera (0,0,-5) (0,0,0)
    --invar   <- newMVar $ Input (0,0) [] False
        --input            <- takeMVar invar
        --cam              <- takeMVar camvar
        --let input' = updateInput input events
        --    cam'   = updateCamera cam input input'
        --putMVar invar input'
        --putMVar camvar $ cam'



updateInput :: Input -> [InputEvent] -> Input
updateInput = foldl addEvent


addEvent :: Input -> InputEvent -> Input
addEvent i@(Input _ keys True) (KeyEvent key _ KeyState'Pressed _) = i{_iKeys = key:keys}
addEvent i@(Input _ keys True) (KeyEvent key _ KeyState'Released _) = i{_iKeys = filter (/=key) keys}
addEvent i@(Input _ _ _) (CursorMoveEvent x' y') = i{_iCursor = (x',y'), _iInitialized=True}
addEvent i _ = i


updateCamera :: Camera -> Input -> Input -> Camera
updateCamera c (Input _ _ False) _ = c
updateCamera c _ (Input _ _ False) = c
updateCamera c i i' =
    let ts = 0.1
        rs = 1/180
        dx = realToFrac $ m'x - mx
        dy = realToFrac $ m'y - my
        (mx,my) = _iCursor i
        (m'x,m'y) = _iCursor i'
    in (\cam -> if Key'R `elem` _iKeys i' then identityCamera else cam) $
       translateCamera i' Key'W (0, 0, ts)  $
       translateCamera i' Key'A (-ts, 0, 0) $
       translateCamera i' Key'S (0, 0, -ts) $
       translateCamera i' Key'D (ts, 0, 0)  $
       rotateCamera (dx,dy) rs c


translateCamera :: Input -> Key -> (Double, Double, Double) -> Camera -> Camera
translateCamera i key (tx,ty,tz) c@(Camera (x,y,z) rxyz) =
    if key `elem` _iKeys i
      then Camera (x+tx, y+ty, z+tz) rxyz
      else c


rotateCamera :: (GLfloat, GLfloat) -> GLfloat -> Camera -> Camera
rotateCamera (dx,dy) s (Camera xyz (p, y, r)) =
    Camera xyz (p + (dx' * s'), y + (dy' * s'), r)
        where [dx',dy',s'] = map realToFrac [dx,dy,s]

prepareCube :: IO ()
prepareCube = do
    i <- genObjectName
    bindBuffer ElementArrayBuffer $= Just i
    withArray ndxs $ \ptr -> do
        bufferData ElementArrayBuffer $= (sizeOfUByteList ndxs, ptr, StaticDraw)
    (_,_) <- bindAndBufferVerts3Colors vs cs
    return ()
{-
renderCube :: Renderer -> IO ()
renderCube r = do
    r^.shader.setIs3d $ True
    r^.shader.setIsTextured $ False
    r^.shader.setColorIsReplaced $ False
    clientState VertexArray $= Enabled
    r^.shader.setProjection $ perspectM44 45 aspr 0.1 10
    --r^.shader.setProjection $ toList $ orthoM44 (-10) 10 (-10) 10 (-10) 10
    r^.shader.setModelview $ toMatrix cam'
    drawElements Triangles (fromIntegral $ length ndxs) UnsignedByte nullPtr
-}

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

