module OpenGL.Program  where

import qualified Graphics.Rendering.OpenGL as GL
import OpenGL.Shader
import Data.Matrix

data Program = Program {
  vertexShader :: ShaderInfo,
  fragmentShader :: ShaderInfo,
  glProgram :: GL.Program
}

createProgram :: String -> String -> IO Program
createProgram vertexShaderPath fragmentShaderPath = do
  glProgram <- loadShaders shaderInfo
  return $ Program vertexShaderInfo fragmentShaderInfo glProgram
  where 
    shaderInfo = [vertexShaderInfo, fragmentShaderInfo]
    vertexShaderInfo = ShaderInfo GL.VertexShader (FileSource vertexShaderPath)
    fragmentShaderInfo = ShaderInfo GL.FragmentShader (FileSource fragmentShaderPath) 

useProgram :: Program -> IO ()
useProgram program = do
  GL.currentProgram GL.$= Just (glProgram program) 

defaultProgram = OpenGL.Program.createProgram "/Users/aravindhs/Aravindh/projects/haskell/fun/shaders/vertex.vert" "/Users/aravindhs/Aravindh/projects/haskell/fun/shaders/fragment.frag"

setUniform :: (GL.Uniform a) => String -> a -> IO ()
setUniform name datum = do
  prog <- defaultProgram
  transformLocation <- GL.uniformLocation (glProgram prog) name
  GL.uniform transformLocation GL.$= datum

setMVPMatrix :: Matrix Float -> IO ()
setMVPMatrix matrix = do
  datum <- GL.newMatrix GL.ColumnMajor (toList matrix) :: IO (GL.GLmatrix GL.GLfloat)
  setUniform "transform" datum

