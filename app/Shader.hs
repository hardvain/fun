module Shader (
  ShaderType(..),
  Shader(..),
  addVertexShader,
  addFragmentShader
) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB

newtype ShaderId = ShaderId GL.GLuint

data ShaderType = Vertex | Fragment deriving Show
data Shader = 
  VertexShader ShaderId
  | FragmentShader ShaderId


addVertexShader :: String -> IO (Maybe GL.Shader)
addVertexShader vertexSource = addShader vertexSource Vertex

addFragmentShader :: String -> IO (Maybe GL.Shader)
addFragmentShader fragmentSource = addShader fragmentSource Fragment

addShader :: String -> ShaderType -> IO (Maybe GL.Shader)
addShader shaderPath shaderType = do
  shader <- GL.createShader (toGLShader shaderType)
  shaderSource <- loadShader shaderPath
  (GL.shaderSourceBS shader) GL.$= shaderSource
  GL.compileShader shader
  status <- GL.compileStatus shader
  log <- GL.shaderInfoLog shader
  _ <- putStrLn ((show shaderType) ++ "\n" ++ log)
  if status then (return $ Just shader) else return Nothing
  
toGLShader :: ShaderType -> GL.ShaderType
toGLShader Vertex = GL.VertexShader
toGLShader Fragment = GL.FragmentShader

loadShader :: String -> IO BS.ByteString
loadShader = BS.readFile