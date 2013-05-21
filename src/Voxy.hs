module Voxy where

import qualified Graphics.UI.GLFW   as GLFW 
import Graphics.Rendering.OpenGL

import Shaders
import Util

import Control.Monad    ( void, when, unless, forever )
import System.Exit      ( exitSuccess )
import System.Directory ( getCurrentDirectory )
import System.FilePath  ( (</>) )
import Foreign.Marshal.Array ( withArray )
import Foreign.Ptr      ( nullPtr )

data Shaders = Shaders { vertexShader   :: VertexShader
                       , fragmentShader :: FragmentShader
                       , program        :: Program
                       , fadeFactorU    :: UniformLocation
                       , texturesU      :: [UniformLocation]
                       , positionA      :: AttribLocation }

voxy :: FilePath -> IO ()
voxy dataDir = do
    putStrLn "Starting voxy..."

    True <- GLFW.initialize
    -- Get a 640 x 480 window.
    -- Initialize the window.
    True <- GLFW.openWindow displayOptions 
    -- Make sure the window is gpu'able.
    True <- GLFW.windowIsHardwareAccelerated
    -- Window will show at upper left corner.
    GLFW.setWindowPosition 0 0
    -- Set the window title.
    GLFW.setWindowTitle "voxy"
    -- Register our scene drawing function.
    GLFW.setWindowRefreshCallback drawScene
    -- Register our resize window function.
    --GLFW.setWindowRefreshCallback drawScene
    -- Register our keyboard input function.
    GLFW.setKeyCallback keyPressed
    -- Register our mouse position input function.
    GLFW.setMousePositionCallback mouseMoved
    -- Register our mouse button input function.
    GLFW.setMouseButtonCallback mouseButtonChanged
    -- Register our window close function.
    GLFW.setWindowCloseCallback shutdown

    -- Shader stuff.
    v   <- loadShader $ dataDir </> "shaders" </> "hello.vert" :: IO VertexShader
    f   <- loadShader $ dataDir </> "shaders" </> "hello.frag" :: IO FragmentShader
    [p] <- genObjectNames 1
    attachedShaders p $= ([v],[f])    
    attribLocation p "position" $= AttribLocation 0 
    linkProgram p
    linked <- get $ linkStatus p
    unless linked $ do
        programLog <- get $ programInfoLog p
        putStrLn programLog

    -- Use this program.
    currentProgram $= Just p
    validateProgram p

    -- Vertex data things.
    [vao] <- genObjectNames 1
    bindVertexArrayObject $= Just vao

    [vbo] <- genObjectNames 1
    bindBuffer ArrayBuffer $= Just vbo
    withArray vertexData $ \ptr ->
        bufferData ArrayBuffer $= (3*3*4, ptr, StaticDraw)
    vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 3 Float 0 nullPtr) 
    clientState VertexArray $= Enabled
--
--    vbo <- alloca $ \ptr -> glGenBuffers 1 ptr >> peek ptr
--    glBindBuffer gl_ARRAY_BUFFER vbo
--    withArray vertexData $ \ptr -> glBufferData gl_ARRAY_BUFFER (3*3*4) ptr gl_STATIC_DRAW
--
--    glVertexAttribPointer 0 3 gl_FLOAT 0 0 nullPtr
--    glEnableVertexAttribArray 0

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

vertexData :: [GLfloat]
vertexData = [  0.0,  0.9, 0.0
             , -0.9, -0.9, 0.0
             ,  0.9, -0.9, 0.0 ]

drawScene :: IO ()
drawScene = do
    -- Clear the screen and the depth buffer.
    clear [ColorBuffer, DepthBuffer]
    drawArrays Triangles 0 3 
    GLFW.swapBuffers

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
