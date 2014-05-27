{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Prelude hiding ((.), id)
import           Urza
import           Cube
import           Graph
import           Tween
import           FRP.Netwire
import           Control.Wire
import qualified Urza.Color as Color
import           Foreign hiding (void)
import           Control.Concurrent.MVar
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)
import           Control.Lens hiding ((#))
import qualified Control.Monad as M
import           System.Directory
import           System.FilePath ((</>))
import           System.Exit
import           Linear hiding (trace)


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


duhWire :: Wire (Timed NominalDiffTime ()) () IO a String
duhWire = for 5 . "hello" <|> for 10 . "goodbye"


runWireUntilInhibited :: (Show s, Show b) => Session IO s -> Wire s () IO () b -> IO ()
runWireUntilInhibited session wire = do
    (ds, session') <- stepSession session
    (mx, wire') <- stepWire wire ds (Right ())
    case mx of 
        Left _  -> return ()
        Right v -> do putStrLn ""
                      putStrLn $ "value: " ++ show v ++ " ds: " ++ show ds
                      runWireUntilInhibited session' wire'


main :: IO ()
main = do
    let txt = map toEnum [32..126]

    camvar  <- newMVar $ Camera (0,0,-5) (0,0,0)
    wvar    <- initUrza (100,100) (800,600) "Voxy Town"
    invar   <- newMVar $ Input (0,0) [] False
    wirevar <- newMVar (clockSession_, posWire) 
    fontDir <- fmap (</> "Library" </> "Fonts") getHomeDirectory
    imgDir  <- fmap (</> "assets" </> "img") getCurrentDirectory

    r <- loadText txt =<< makeRenderer (fontDir </> "UbuntuMono-R.ttf") 24
    Just graph <- graphFromFile (imgDir </> "oryx_roguelike_16x24.png")
    Just bmp <- loadBitmap (imgDir </> "photo.png")

    renderCube
    M.forever $ do
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
        blend $= Enabled
        blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
        clear [ColorBuffer, DepthBuffer]

        r^.shader.setIs3d $ True
        r^.shader.setIsTextured $ False
        r^.shader.setColorIsReplaced $ False
        clientState VertexArray $= Enabled
        r^.shader.setProjection $ perspectM44 45 aspr 0.1 10
        --r^.shader.setProjection $ toList $ orthoM44 (-10) 10 (-10) 10 (-10) 10
        r^.shader.setModelview $ toMatrix cam'
        drawElements Triangles (fromIntegral $ length ndxs) UnsignedByte nullPtr

        r^.shader.setProjection $ orthoM44 0 (fromIntegral winW) 0 (fromIntegral winH) 0 1
        r^.shader.setModelview $ eye4
        r^.shader.setTextColor $ Color4 1 1 0 1
        
        -- Netwire experiments.
        (session, wire) <- takeMVar wirevar
        (ds, session')  <- stepSession session
        (mx, wire')     <- stepWire wire ds (Right ())
        putMVar wirevar (session', wire')

        case mx of
            Left _ -> return ()
            Right pos -> do
                drawTextAt r pos $ show pos
                drawBitmapPixels r bmp (Rectangle 123 192 97 45) pos (Size 97 45) (Scale 2 2) (Rotation 0)
        drawGraph r graph

        swapBuffers window
        shouldClose <- windowShouldClose window
        M.when shouldClose exitSuccess

--posWire = pure $ Position 0 $ round (0.0 :: Double)

--posWire :: Wire (Timed NominalDiffTime ()) () IO a Position 
--posWire = for 1 . pure (Position 0 0) --> linearTween (Position 0 0) (Position 100 100) 3.0

posWire :: (HasTime t s, Monad m, Fractional t) => Wire s () m a Position
posWire = Position <$> tween <*> tween
    where tween = fmap round (latestTween 0 200 1)


