module Program (
  createProgram,
  useProgram,
  Program(..),
  VertexDescriptor,
  VertexAttributeDescriptor(..)
) where
import qualified Graphics.Rendering.OpenGL as GL
import Shader


type Dimension = Int
type Format = GL.DataType
data VertexAttributeDescriptor = VertexAttributeDescriptor GL.AttribLocation Format Dimension GL.Offset

type VertexDescriptor = [VertexAttributeDescriptor]


data Program = Program {
  vertexShader :: ShaderInfo,
  glProgram :: GL.Program,
  fragmentShader :: ShaderInfo,
  vertexDescriptor :: VertexDescriptor
}

createProgram :: String -> String -> VertexDescriptor -> IO Program
createProgram vertexShaderPath fragmentShaderPath vertexDescriptor = do
  glProgram <- loadShaders [vertexShaderInfo, fragmentShaderInfo]
  return Program {
    vertexShader = vertexShaderInfo,
    fragmentShader = fragmentShaderInfo,
    glProgram = glProgram,
    vertexDescriptor = vertexDescriptor
  }
  where 
    vertexShaderInfo = ShaderInfo GL.VertexShader (FileSource vertexShaderPath)
    fragmentShaderInfo = ShaderInfo GL.FragmentShader (FileSource fragmentShaderPath) 

useProgram :: Program -> IO ()
useProgram program = do
  GL.currentProgram GL.$= Just (glProgram program)
