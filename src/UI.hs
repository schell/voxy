module Main where

import Wire.Core
import Render
import Linear
import Urza hiding (color)
import Graphics.Rendering.OpenGL hiding (color)
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import System.Exit
import System.Directory
import Data.Time.Clock
import qualified Data.Set as S

data Button = Button { btnTitle :: String
                     , btnColor :: Color4 Double
                     , btnTransform :: Transform2d Double
                     } deriving (Show)

data App = App { appSize :: V2 Int
               , appBtn  :: Button
               } deriving (Show)

type InputEnvR = Reader InputEnv

btnTfrm :: Wire InputEnvR () (Transform2d Double)
btnTfrm = Transform2d <$> btnPos <*> btnSize <*> scl <*> pure 0
    where scl = pure $ V2 1 1

btnPos :: Wire InputEnvR () (V2 Double)
btnPos = pure $ V2 10 10

btnSize :: Wire InputEnvR () (V2 Double)
btnSize = pure $ V2 100 32

btnClr :: Wire InputEnvR () (Color4 Double)
btnClr = Wire $ \dt _ -> do
    V2 x y  <- execWire btnPos dt ()
    V2 w h  <- execWire btnSize dt ()
    (mx,my) <- asks _ienvLastCursorPos
    mbsdown <- asks _ienvMouseButtonsDown
    return $ if V2 mx my `pointInRect` Rectangle x y w h
               then if S.member MouseButton'1 mbsdown
                      then Output (Color4 0 0 1 1) btnClr
                      else Output (Color4 0 1 0 1) btnClr
               else Output (Color4 1 0 0 1) btnClr

btnWire :: Wire InputEnvR () Button
btnWire = Button <$> title <*> color <*> btnTfrm
    where title = pure "Button"
          color = btnClr

windowSize :: Wire InputEnvR () (V2 Int)
windowSize = Wire $ \_ _ -> do
    s <- asks _ienvWindowSize
    return $ Output s windowSize

appWire :: Wire InputEnvR () App
appWire = App <$> windowSize
              <*> btnWire

render :: App -> Renderer -> IO ()
render (App (V2 w h) (Button title c t2d)) rndr = do
    let s = _shader rndr
        Transform2d (V2 x y) (V2 bw bh) _ _ = t2d
        frame = Rectangle x y bw bh
    renderViewport s w h $ Color4 0 0 0 0
    fillPath_ s $ do
        setColor c
        rectangle frame
    _setTextColor s $ Color4 1 1 1 1
    _ <- drawTextAt' rndr (Position (floor x) (floor y)) title
    return ()

loop :: WindowVar -> Renderer -> InputEnv -> UTCTime -> Wire (Reader InputEnv) () App -> IO ()
loop wvar rndr env t w = do
    pollEvents
    (events, window) <- takeMVar wvar
    putMVar wvar ([], window)
    makeContextCurrent $ Just window

    t' <- getCurrentTime

    let dt   = realToFrac $ diffUTCTime t' t
        env' = foldl foldInput env events
        Output app w' = runReader (stepWire w dt ()) env'

    render app rndr
    swapBuffers window

    shouldClose <- windowShouldClose window
    when shouldClose exitSuccess

    loop wvar rndr env' t' w'

main :: IO ()
main = do
    wvar <- initUrza (0,0) (600,600) "UI"
    cwd  <- getCurrentDirectory
    rndr <- makeAsciiRenderer (cwd ++ "/assets/font/UbuntuMono-B.ttf") 16
    t    <- getCurrentTime
    loop wvar rndr emptyInputEnv t appWire
