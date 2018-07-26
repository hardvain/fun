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

data ShaderType = Vertex | Fragment
data Shader = 
  VertexShader ShaderId
  | FragmentShader ShaderId


addVertexShader :: String -> IO GL.Shader
addVertexShader vertexSource = addShader vertexSource Vertex

addFragmentShader :: String -> IO GL.Shader
addFragmentShader fragmentSource = addShader fragmentSource Fragment

addShader :: String -> ShaderType -> IO GL.Shader
addShader shaderPath shaderType = do
  shader <- GL.createShader (toGLShader shaderType)
  shaderSource <- loadShader shaderPath
  (GL.shaderSourceBS shader) GL.$= shaderSource
  GL.compileShader shader
  return shader
  
toGLShader :: ShaderType -> GL.ShaderType
toGLShader Vertex = GL.VertexShader
toGLShader Fragment = GL.FragmentShader

loadShader :: String -> IO BS.ByteString
loadShader = BS.readFile