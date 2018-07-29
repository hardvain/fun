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


vertices :: [Vertex4 Float]
vertices = [
  Vertex4 (-0.90) (-0.90) 0 1.0,
  Vertex4   0.85  (-0.90) 0 1.0,
  Vertex4 (-0.90)   0.85 0 1.0,
  Vertex4   0.90  (-0.85) 0 1.0,
  Vertex4   0.90    0.90 0 1.0,
  Vertex4 (-0.85)   0.90 0 1.0]

colorsData :: [Color4 Float]
colorsData = [
  Color4   0.90  0.50  0.5   0.1,  -- Triangle 1
  Color4   0.35  0.14  0.5   0.2,
  Color4   0.50  0.62  0.5   0.6,
  Color4   0.60  0.0   0.5   0.75,  -- Triangle 2
  Color4   0.20  0.20  0.5   0.04,
  Color4   0.15  0.49  0.5   0.23]

program = Program.createProgram "/Users/aravindhs/Aravindh/projects/haskell/fun/shaders/vertex.shader" "/Users/aravindhs/Aravindh/projects/haskell/fun/shaders/fragment.shader"


main :: IO ()
main = do
  win <- Window.createWindow 1920 1280 "Fun"
  program >>= useProgram
  draw (colorsData, vertices) win
  Window.closeWindow win
  