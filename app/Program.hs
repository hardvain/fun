module Program  where
import qualified Graphics.Rendering.OpenGL as GL
import Shader

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