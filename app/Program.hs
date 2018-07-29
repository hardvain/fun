module Program (
  createProgram,
  useProgram,
  Program(..),
  VertexDescriptor,
  VertexAttributeDescriptor(..)
) where
import qualified Graphics.Rendering.OpenGL as GL
import Shader
import Foreign.Ptr


type Dimension = Int
type Format = GL.DataType
data VertexAttributeDescriptor = VertexAttributeDescriptor GL.AttribLocation Format Dimension GL.Offset deriving Show

type VertexDescriptor = [VertexAttributeDescriptor]


data Program = Program {
  vertexShader :: ShaderInfo,
  glProgram :: GL.Program,
  fragmentShader :: ShaderInfo,
  vertexDescriptor :: VertexDescriptor
}

data PipelineState = PipelineState Program

-- setPipelineState :: PipelineState -> IO()
-- setPipelineState pipelineState = do
  
bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

registerVertexAttributeDescriptor :: VertexAttributeDescriptor -> IO ()
registerVertexAttributeDescriptor v@(VertexAttributeDescriptor attribLocation format dimension offset) = do 
  GL.vertexAttribPointer attribLocation GL.$=
    (GL.ToFloat, GL.VertexArrayDescriptor (fromIntegral dimension) format 0 (bufferOffset 0))

processVertexDescriptors :: VertexDescriptor -> [IO()]
processVertexDescriptors vertexDescriptor = do
  map registerVertexAttributeDescriptor vertexDescriptor

createProgram :: String -> String -> VertexDescriptor -> IO Program
createProgram vertexShaderPath fragmentShaderPath vertexDescriptor = do
  glProgram <- loadShaders [vertexShaderInfo, fragmentShaderInfo]
  result <- (processVertexDescriptors vertexDescriptor) !! 0
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
  -- let descriptor = (vertexDescriptor program)
  -- (fmap (\ad -> GL.vertexAttribArray (getAttribLocation ad) GL.$= GL.Enabled) descriptor)!!0
  GL.currentProgram GL.$= Just (glProgram program) 

getAttribLocation :: VertexAttributeDescriptor -> GL.AttribLocation
getAttribLocation (VertexAttributeDescriptor location _ _ _) = location