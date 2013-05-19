module Main where

import qualified Graphics.UI.GLFW as GLFW 

import Shaders
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw           ( gluPerspective )
import Data.Bits                            ( (.|.) )
import System.Exit                          ( exitSuccess ) 
import Control.Monad                        ( forever, void )

main :: IO ()
main = do
    True <- GLFW.initialize
    -- Get a 640 x 480 window.
    -- Initialize the window.
    True <- GLFW.openWindow displayOptions 
    -- Make sure the window is gpu'able.
    True <- GLFW.windowIsHardwareAccelerated
    -- Window will show at upper left corner.
    GLFW.setWindowPosition 0 0
    -- Set the window title.
    GLFW.setWindowTitle "Voxy"
    -- Register our scene drawing function.
    GLFW.setWindowRefreshCallback drawScene
    -- Register our resize window function.
    GLFW.setWindowRefreshCallback drawScene
    -- Register our keyboard input function.
    GLFW.setKeyCallback keyPressed
    -- Register our mouse position input function.
    GLFW.setMousePositionCallback mouseMoved
    -- Register our mouse button input function.
    GLFW.setMouseButtonCallback mouseButtonChanged
    -- Register our window close function.
    GLFW.setWindowCloseCallback shutdown

    -- Shader stuff.
    v <- compileVertexShader vertexShaderSource
    f <- compileFragmentShader fragmentShaderSource

    initGL

    forever drawScene

displayOptions :: GLFW.DisplayOptions
displayOptions = GLFW.defaultDisplayOptions { GLFW.displayOptions_width  = 800
                                            , GLFW.displayOptions_height = 600
                                            -- Set depth buffering and RGBA colors
                                            , GLFW.displayOptions_numRedBits   = 8
                                            , GLFW.displayOptions_numGreenBits = 8
                                            , GLFW.displayOptions_numBlueBits  = 8
                                            , GLFW.displayOptions_numAlphaBits = 8
                                            , GLFW.displayOptions_numDepthBits = 1
                                            -- , GLFW.displayOptions_displayMode = GLFW.Fullscreen
                                            }

drawScene :: IO ()
drawScene = do
    -- Clear the screen and the depth buffer.
    glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)
    glDrawArrays gl_TRIANGLES 0 3
    GLFW.swapBuffers

resizeScene :: GLFW.WindowSizeCallback
-- Prevent a division by zero.
resizeScene w 0 = resizeScene w 1 
resizeScene width height = do
    glViewport 0 0 (fromIntegral width) (fromIntegral height)
    glMatrixMode gl_PROJECTION
    glLoadIdentity
    gluPerspective 45 (fromIntegral width/fromIntegral height) 0.1 100
    glMatrixMode gl_MODELVIEW
    glLoadIdentity
    glFlush

keyPressed :: GLFW.KeyCallback
keyPressed GLFW.KeyEsc True = void shutdown
keyPressed a b = putStrLn $ isPressedString a b

mouseButtonChanged :: GLFW.MouseButtonCallback
mouseButtonChanged a b = putStrLn $ isPressedString a b

mouseMoved :: GLFW.MousePositionCallback
mouseMoved x y = putStrLn $ show x ++ ", " ++ show y

isPressedString :: (Show a) => a -> Bool -> String
isPressedString button pressed = show button ++ " is " ++ (if pressed then "pressed" else "not pressed") ++ "."

shutdown :: GLFW.WindowCloseCallback 
shutdown = do
    GLFW.closeWindow
    GLFW.terminate
    exitSuccess 
    return True

initGL :: IO ()
initGL = do
    -- Smooth color shading.
    glShadeModel gl_SMOOTH
    -- Clear to black.
    glClearColor 0 0 0 0
    -- Enable clearing of depth buffer.
    glEnable gl_DEPTH_TEST
    -- Set the type of depth test.
    glDepthFunc gl_LEQUAL
    glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST

