module Main where

import Wire.Core
import Wire.Tween
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
import qualified Data.Set as S

data Button = Button { btnTitle :: String
                     , btnColor :: Color4 Double
                     , btnTransform :: Transform2d Double
                     } deriving (Show)

data ButtonState = BtnNormal
                 | BtnHover
                 | BtnDown
                 deriving (Eq)

data App = App { appSize :: V2 Int
               , appBtn  :: Button
               , appPnt  :: V2 Double
               } deriving (Show)

type InputEnvR = Reader InputEnv

btnTfrm :: Wire InputEnvR () (Transform2d Double)
btnTfrm = Transform2d <$> btnPos <*> btnSize <*> scl <*> pure 0
    where scl = pure $ V2 1 1

btnPos :: Wire InputEnvR () (V2 Double)
btnPos = pure $ V2 10 10

btnSize :: Wire InputEnvR () (V2 Double)
btnSize = pure $ V2 100 32

btnState :: Wire InputEnvR () ButtonState
btnState = Wire $ \dt _ -> do
    V2 x y  <- execWire btnPos dt ()
    V2 w h  <- execWire btnSize dt ()
    (mx,my) <- asks _ienvLastCursorPos
    mbsdown <- asks _ienvMouseButtonsDown
    let state = if V2 mx my `pointInRect` Rectangle x y w h
                  then if S.member MouseButton'1 mbsdown
                         then BtnDown
                         else BtnHover
                  else BtnNormal
    return $ Output state btnState

btnClr :: Wire InputEnvR () (Color4 Double)
btnClr = Wire $ \dt _ -> do
    state <- execWire btnState dt ()
    let c = case state of
                BtnNormal -> Color4 1 0 0 1
                BtnHover  -> Color4 0 1 0 1
                BtnDown   -> Color4 0 0 1 1
    return $ Output c btnClr

btnWire :: Wire InputEnvR () Button
btnWire = Button <$> title <*> color <*> btnTfrm
    where title = pure "Button"
          color = btnClr

windowSize :: Wire InputEnvR () (V2 Int)
windowSize = Wire $ \_ _ -> do
    s <- asks _ienvWindowSize
    return $ Output s windowSize

cursor :: Wire InputEnvR () (V2 Double)
cursor = Wire $ \_ _ -> do
    (x, y) <- asks _ienvLastCursorPos
    return $ Output (V2 x y) cursor

point :: Wire InputEnvR () (V2 Double)
point = switch moveBackAndForth
    where switch w = Wire $ \dt _ -> do
              s <- execWire btnState dt ()
              if s /= BtnDown
                then return $ Output (V2 0 0) $ switch w
                else stepWire w dt ()
          moveBackAndForth = for 4 moveThere --> (for 4 moveBack --> point)
          moveThere = V2 <$> linear 0 100 4 <*> linear 0 100 4
          moveBack  = V2 <$> linear 100 0 4 <*> linear 100 0 4
--point = btnState ~> switchState
--    where switchState = pureWire $ \s -> case s of
--                                             BtnNormal -> V2 0 0
--                                             BtnHover  -> V2 0 0
--                                             BtnDown   -> V2 100 100

appWire :: Wire InputEnvR () App
appWire = App <$> windowSize
              <*> btnWire
              <*> point

render :: App -> Renderer -> IO ()
render (App (V2 w h) (Button title c t2d) (V2 px py)) rndr = do
    let s = _shader rndr
        Transform2d (V2 x y) (V2 bw bh) _ _ = t2d
        frame = Rectangle x y bw bh
    renderViewport s w h $ Color4 0 0 0 0
    fillPath_ s $ do
        setColor c
        rectangle frame
    fillPath_ s $ do
        setColor $ Color4 1 0 1 1
        rectangle $ Rectangle px py 10 10
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
