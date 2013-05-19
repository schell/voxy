module Shaders where

import Graphics.Rendering.OpenGL.Raw
import Control.Monad                    ( when, unless )
import Foreign.C.String                 ( withCString, peekCAString )
import Foreign.Marshal.Alloc            ( alloca, allocaBytes )
import Foreign.Marshal.Array            ( withArray )
import Foreign.Ptr                      ( nullPtr )
import Foreign.Storable                 ( peek )

data  ShaderType = ShaderVertex | ShaderFragment deriving (Show, Eq)
type ShaderID    = GLuint

compileVertexShader :: String -> IO ShaderID
compileVertexShader = compileShader ShaderVertex 

compileFragmentShader :: String -> IO ShaderID
compileFragmentShader = compileShader ShaderFragment 

compileShader :: ShaderType -> String -> IO ShaderID
compileShader shaderType src = 
    let (shaderName,glShader) = case shaderType of
                                    ShaderVertex -> ("vertex", gl_VERTEX_SHADER)
                                    ShaderFragment -> ("fragment", gl_FRAGMENT_SHADER) in do
        putStr "\n"
        -- Create a new shader to store into.
        s <- glCreateShader glShader 
        putStrLn $ "Created "++shaderName++" shader."
        -- Replace its source.
        withCString src $ \str -> 
            withArray [str] $ \ptr ->
                glShaderSource s 1 ptr nullPtr
        putStrLn $ "Replaced "++shaderName++" shader source."
        -- Compile and check the log for errors.
        glCompileShader s
        success <- shaderCompilationSuccessful s
        when success $ putStrLn $ "Compiled "++shaderName++" shader."
        unless success $ do
            shaderLog <- getShaderLog s 256 
            putStrLn shaderLog
        return s


shaderCompilationSuccessful :: ShaderID -> IO Bool
shaderCompilationSuccessful shaderId = do 
    compileLogLength <- alloca $ \statusPtr -> do
        glGetShaderiv shaderId gl_INFO_LOG_LENGTH statusPtr 
        peek statusPtr
    return $ compileLogLength == 0

getShaderLog :: ShaderID -> GLsizei -> IO String
getShaderLog shaderId logLength = allocaBytes (fromIntegral logLength) $ \logPtr -> do
    glGetShaderInfoLog shaderId logLength nullPtr logPtr
    peekCAString logPtr

vertexShaderSource :: String 
vertexShaderSource = unlines
  [ "attribute vec3 position;"
  , "void main() {"
  , "   gl_Position = vec4(position, 1);"
  , "}"
  ]
 
fragmentShaderSource :: String
fragmentShaderSource = unlines
  [ "void main() {"
  , "   gl_FragColor = vec4(1, 1, 1, 1);"
  , "}"
  ]

