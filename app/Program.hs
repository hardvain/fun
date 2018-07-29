module Program  where
import qualified Graphics.Rendering.OpenGL as GL
import Shader

data Program = Program {
  vertexShader :: ShaderInfo,
  glProgram :: GL.Program,
  fragmentShader :: ShaderInfo
}


createProgram :: String -> String -> IO Program
createProgram vertexShaderPath fragmentShaderPath = do
  glProgram <- loadShaders [vertexShaderInfo, fragmentShaderInfo]
  return Program {
    vertexShader = vertexShaderInfo,
    fragmentShader = fragmentShaderInfo,
    glProgram = glProgram
  }
  where 
    vertexShaderInfo = ShaderInfo GL.VertexShader (FileSource vertexShaderPath)
    fragmentShaderInfo = ShaderInfo GL.FragmentShader (FileSource fragmentShaderPath) 

useProgram :: Program -> IO ()
useProgram program = do
  GL.currentProgram GL.$= Just (glProgram program) 