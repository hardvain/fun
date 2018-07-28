module Main where
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad
import System.Exit ( exitWith, ExitCode(..) )
import Shader
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Window
import Buffer 
import Program
import Renderer

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

vertices :: [Vertex2 GLfloat]
vertices = [
  Vertex2 (-0.90) (-0.90),  -- Triangle 1
  Vertex2   0.85  (-0.90),
  Vertex2 (-0.90)   0.85 ,
  Vertex2   0.90  (-0.85),  -- Triangle 2
  Vertex2   0.90    0.90 ,
  Vertex2 (-0.85)   0.90 ]

numVertices = length vertices

descriptor :: VertexDescriptor
descriptor = [ VertexAttributeDescriptor (AttribLocation 0) Float 2 0 ]

program = Program.createProgram "/Users/aravindhs/Aravindh/projects/haskell/fun/shaders/vertex.shader" "/Users/aravindhs/Aravindh/projects/haskell/fun/shaders/fragment.shader" descriptor


initResources :: IO Descriptor
initResources = do
  triangles <- createVertexArrayObject
  let firstIndex = 0
  let vPosition = AttribLocation 0
  withVertexObject triangles $ do
    createArrayBuffer vertices 
    vertexAttribPointer vPosition $=
      (ToFloat, VertexArrayDescriptor 2 Float 0 (bufferOffset firstIndex))
    vertexAttribArray vPosition $= Enabled
  program >>= useProgram

  return $ Descriptor triangles firstIndex (fromIntegral numVertices)

main :: IO ()
main = do
  win <- Window.createWindow 1920 1280 "Fun"
  descriptor <- initResources
  _ <- render win descriptor
  GLFW.destroyWindow win
  GLFW.terminate

