module Shader (
  addVertexShader,
  addFragmentShader
) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.ByteString as BS

addVertexShader :: String -> IO (Maybe GL.Shader)
addVertexShader vertexSource = addShader vertexSource GL.VertexShader

addFragmentShader :: String -> IO (Maybe GL.Shader)
addFragmentShader fragmentSource = addShader fragmentSource GL.FragmentShader

createShader :: GL.ShaderType -> IO GL.Shader
createShader = GL.createShader

setShaderSource :: GL.Shader -> BS.ByteString -> IO ()
setShaderSource shader shaderSource = (GL.shaderSourceBS shader) GL.$= shaderSource

compileShader :: GL.Shader -> IO()
compileShader = GL.compileShader

getCompileStatus :: GL.Shader -> IO Bool
getCompileStatus = GL.compileStatus

getShaderLog :: GL.Shader -> IO String
getShaderLog = GL.shaderInfoLog

loadShaderSource :: String -> IO BS.ByteString
loadShaderSource = BS.readFile

addShader :: String -> GL.ShaderType -> IO (Maybe GL.Shader)
addShader shaderPath shaderType = do
  shader <- createShader shaderType
  shaderSource <- loadShaderSource shaderPath
  setShaderSource shader shaderSource
  compileShader shader
  status <- getCompileStatus shader
  log <- getShaderLog shader
  if status then (return $ Just shader) else return Nothing

