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


initResources :: IO Descriptor
initResources = do
  triangles <- genObjectName
  bindVertexArrayObject $= Just triangles

  let vertices = [
        Vertex2 (-0.90) (-0.90),  -- Triangle 1
        Vertex2   0.85  (-0.90),
        Vertex2 (-0.90)   0.85 ,
        Vertex2   0.90  (-0.85),  -- Triangle 2
        Vertex2   0.90    0.90 ,
        Vertex2 (-0.85)   0.90 ] :: [Vertex2 GLfloat]
      numVertices = length vertices

  _ <- createBuffer vertices ArrayBuffer

  program <- Program.createProgram "/Users/aravindhs/Aravindh/projects/haskell/fun/shaders/vertex.shader" "/Users/aravindhs/Aravindh/projects/haskell/fun/shaders/fragment.shader"
  
  useProgram program

  let firstIndex = 0
      vPosition = AttribLocation 0
  vertexAttribPointer vPosition $=
    (ToFloat, VertexArrayDescriptor 2 Float 0 (bufferOffset firstIndex))
  vertexAttribArray vPosition $= Enabled

  return $ Descriptor triangles firstIndex (fromIntegral numVertices)

main :: IO ()
main = do
  win <- Window.createWindow 1920 1280 "Fun"
  descriptor <- initResources
  _ <- render win descriptor
  GLFW.destroyWindow win
  GLFW.terminate
