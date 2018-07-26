module Shader (
  ShaderType(..),
  Shader(..),
  addVertexShader,
  addFragmentShader
) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Data.ByteString as BS

newtype ShaderId = ShaderId GL.GLuint

data ShaderType = Vertex | Fragment
data Shader = 
  VertexShader ShaderId
  | FragmentShader ShaderId


addVertexShader :: BS.ByteString -> IO GL.Shader
addVertexShader vertexSource = addShader vertexSource Vertex

addFragmentShader :: BS.ByteString -> IO GL.Shader
addFragmentShader fragmentSource = addShader fragmentSource Fragment

addShader :: BS.ByteString -> ShaderType -> IO GL.Shader
addShader shaderSource shaderType = do
  shader <- GL.createShader (toGLShader shaderType)
  (GL.shaderSourceBS shader) GL.$= shaderSource
  GL.compileShader shader
  return shader
  
toGLShader :: ShaderType -> GL.ShaderType
toGLShader Vertex = GL.VertexShader
toGLShader Fragment = GL.FragmentShader