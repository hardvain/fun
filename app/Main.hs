module Main where
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad
import System.Exit ( exitWith, ExitCode(..) )
import Shader
import Foreign.Marshal.Array
import Foreign.Ptr
import Window
import Foreign.Storable
import Window
import Buffer 
import Program
import Renderer


vertices :: [Vertex3 GLfloat]
vertices = [
  Vertex3 (-0.90) (-0.90) 0,  -- Triangle 1
  Vertex3   0.85  (-0.90) 0,
  Vertex3 (-0.90)   0.85 0,
  Vertex3   0.90  (-0.85) 0,  -- Triangle 2
  Vertex3   0.90    0.90 0,
  Vertex3 (-0.85)   0.90 0]

numVertices = length vertices

program = Program.createProgram "/Users/aravindhs/Aravindh/projects/haskell/fun/shaders/vertex.shader" "/Users/aravindhs/Aravindh/projects/haskell/fun/shaders/fragment.shader"


initResources :: IO Mesh
initResources = createMesh vertices


main :: IO ()
main = do
  win <- Window.createWindow 1920 1280 "Fun"
  descriptor <- initResources
  program >>= useProgram
  _ <- render win descriptor
  GLFW.destroyWindow win
  GLFW.terminate



createMesh :: Positions -> IO Mesh
createMesh positions = do
  vao <- createVertexArrayObject
  positionBufferObject <- withVertexArrayObject vao $ do
    positionBufferObject <- createPositionBuffer positions
    positionAttributeLocation <- describeAttribute 0 3 GL.Float
    return positionBufferObject
  return Mesh {
    positionBufferObject = positionBufferObject,
    positions = positions,
    vao = vao
  }
  -- create buffer for position
  -- create buffer for colors
  -- create vbo
  -- bind position buffer
  -- set vertex attrib pointer for position
  -- enable the vertex attrib 
  -- unbind vao
