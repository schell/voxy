module Main where

import Graphics.GPipe
import Graphics.GPipe.Texture.Load
import qualified Data.Vec as Vec
import Data.Vec.Nat
import Data.Monoid
import Data.IORef
import Graphics.UI.GLUT ( Window, mainLoop, postRedisplay, idleCallback, getArgsAndInitialize, ($=) )

main :: IO ()
main = do
    getArgsAndInitialize
    tex <- loadTexture RGB8 "src/gal.png"
    angleRef <- newIORef 0.0
    newWindow "Spinning box"
