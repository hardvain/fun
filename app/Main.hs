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

colorsData :: [Vertex4 GLfloat]
colorsData = [
  Vertex4   0.10  0.50 0.5 1.0,  -- Triangle 1
  Vertex4   0.35  0.14 0.5 1.0,
  Vertex4   0.50  0.62 0.5 1.0,
  Vertex4   0.60  0.0 0.5 1.0,  -- Triangle 2
  Vertex4   0.20  0.20 0.5 1.0,
  Vertex4   0.15  0.49 0.5 1.0]

program = Program.createProgram "/Users/aravindhs/Aravindh/projects/haskell/fun/shaders/vertex.shader" "/Users/aravindhs/Aravindh/projects/haskell/fun/shaders/fragment.shader"


main :: IO ()
main = do
  win <- Window.createWindow 1920 1280 "Fun"
  mesh <- createMesh vertices
  program >>= useProgram
  _ <- render win mesh
  Window.closeWindow win
  
createMesh :: Positions -> IO Mesh
createMesh positions = do
  vao <- createVertexArrayObject
  positionBufferObject <- withVertexArrayObject vao $ do
    positionBufferObject <- createArrayBuffer positions
    let positionDescriptor = VertexAttributeDescriptor {
      attributeLocation = 0,
      dimension = 3
    }
    describeAttribute positionDescriptor
    return positionBufferObject
  colorBufferObject <- withVertexArrayObject vao $ do
    colorBufferObject <- createArrayBuffer colorsData
    let colorDescriptor = VertexAttributeDescriptor {
      attributeLocation = 1,
      dimension = 4
    }
    describeAttribute colorDescriptor
    return colorBufferObject
  return Mesh {
    positionBufferObject = positionBufferObject,
    positions = positions,
    colors = colorsData,
    colorBufferObject = colorBufferObject,
    vao = vao
  }
