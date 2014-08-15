{-# LANGUAGE BangPatterns #-}
module Main where

import Wire
import Render
import Linear hiding (point)
import Urza hiding (color)
import Graphics.Rendering.OpenGL hiding (color)
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import System.Exit
import System.Directory
import Data.Time.Clock
import Data.Maybe
import qualified Data.Set as S

data ButtonState = BtnNormal
                 | BtnHover
                 | BtnDown
                 deriving (Show, Eq)

data Button = Button { btnTitle :: String
                     , btnState :: ButtonState
                     , btnTransform :: Transform2d Double
                     } deriving (Show)

data App = App { appBtn  :: Button
               , appPnt  :: V2 Double
               } deriving (Show)

type InputEnvR = Reader InputEnv

buttonColor :: ButtonState -> Color4 Double
buttonColor BtnNormal = Color4 0.44 0.44 0.44 1.0
buttonColor BtnHover  = Color4 0.55 0.55 0.55 1.0
buttonColor BtnDown   = Color4 0.33 0.33 0.33 1.0

btnTfrm :: Wire InputEnvR () (Transform2d Double)
btnTfrm = Transform2d <$> btnPos <*> btnSize <*> scl <*> pure 0
    where scl = pure $ V2 1 1

btnPos :: Wire InputEnvR () (V2 Double)
btnPos = pure $ V2 10 10

btnSize :: Wire InputEnvR () (V2 Double)
btnSize = pure $ V2 100 32

btnSt :: Wire InputEnvR () ButtonState
btnSt = Wire $ \dt _ -> do
    V2 x y  <- execWire btnPos dt ()
    V2 w h  <- execWire btnSize dt ()
    (mx,my) <- asks _ienvLastCursorPos
    mbsdown <- asks _ienvMouseButtonsDown
    let state = if V2 mx my `pointInRect` Rectangle x y w h
                  then if S.member MouseButton'1 mbsdown
                         then BtnDown
                         else BtnHover
                  else BtnNormal
    return $ Output state btnSt

btnStateChanged :: Wire InputEnvR () (Maybe ButtonState)
btnStateChanged = eventOnChange btnSt

btnWire :: Wire InputEnvR () Button
btnWire = Button <$> title <*> btnSt <*> btnTfrm
    where title = fmap show (holdWith BtnNormal $ btnStateChanged)

windowSize :: Wire InputEnvR () (V2 Int)
windowSize = Wire $ \_ _ -> do
    s <- asks _ienvWindowSize
    return $ Output s windowSize

cursor :: Wire InputEnvR () (V2 Double)
cursor = Wire $ \_ _ -> do
    (x, y) <- asks _ienvLastCursorPos
    return $ Output (V2 x y) cursor

chainPoint :: Wire InputEnvR () (V2 Double)
chainPoint = switchWhen btnSt (== BtnDown)
                 restHere
                 (moveFromUntilRelease $ V2 0 0)
    where restHere  = pure $ V2 0 0
          moveFromUntilRelease p = chainWhen btnSt (== BtnHover)
                                        (moveFrom p) comeBackFrom
          moveFrom (V2 x y) = tweenV2 easeOutExpo (V2 x y) (V2 100 0) (2- x/100 * 2)
          comeBackFrom (V2 x _) = let t = x/100 *2
                                  in chainWhen btnSt (== BtnDown)
                                         (tweenV2 easeOutExpo (V2 x 0) (V2 0 0) t)
                                         moveFromUntilRelease

appWire :: Wire InputEnvR () App
appWire = App <$> btnWire
              <*> chainPoint

renderBtn :: Renderer -> Button -> IO ()
renderBtn rndr (Button title st t2d) = do
    let Transform2d (V2 x y) (V2 bw bh) _ _ = t2d
        frame = Rectangle x y bw bh
        shdr = _shader rndr
    fillPath_ shdr $ do
        setColor $ buttonColor st
        rectangle frame
    _setTextColor shdr $ Color4 1 1 1 1
    let V2 w h = textSize rndr title
        x' = x + (bw/2 - w/2)
        y' = y + (bh/2 - h/2) - 4
    _ <- drawTextAt' rndr (Position (floor x') (floor y')) title
    return undefined

render :: App -> Renderer -> V2 Int -> V2 Int -> IO ()
render (App b (V2 px py)) rndr vps pjs = do
    let s = _shader rndr
    renderViewport s vps pjs $ Color4 0 0 0 0
    renderBtn rndr b
    fillPath_ s $ do
        setColor $ Color4 1 1 1 1
        rectangle $ Rectangle px py 10 10
    strokePath_ s $ do
        setColor $ Color4 1 0 1 1
        rectangle $ Rectangle px py 10 10
    return ()

loop :: WindowVar -> Renderer -> InputEnv -> UTCTime -> Wire (Reader InputEnv) () App -> IO ()
loop wvar rndr env t w = do
    pollEvents
    (events, window) <- takeMVar wvar
    putMVar wvar ([], window)
    makeContextCurrent $ Just window

    (vpw,vph) <- getFramebufferSize window
    (pjw,pjh) <- getWindowSize window

    t' <- getCurrentTime

    let dt   = realToFrac $ diffUTCTime t' t
        env' = foldl foldInput env events
        Output app w' = runReader (stepWire w dt ()) env'

    render app rndr (V2 vpw vph) (V2 pjw pjh)
    swapBuffers window

    shouldClose <- windowShouldClose window
    when shouldClose exitSuccess

    -- Clear out the env's list of events.
    let env'' = env'{ _ienvEvents = [] }

    loop wvar rndr env'' t' w'

main :: IO ()
main = do
    wvar <- initUrza (0,0) (600,600) "UI"
    cwd  <- getCurrentDirectory
    rndr <- makeAsciiRenderer (cwd ++ "/assets/font/UbuntuMono-B.ttf") 16
    t    <- getCurrentTime
    loop wvar rndr emptyInputEnv t appWire
