module Main where
import qualified Graphics.Rendering.OpenGL as GL
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
import Shape

vertices :: [GL.Vertex4 Float]
vertices = [
  GL.Vertex4 (-0.90) (-0.90) 0 1.0,
  GL.Vertex4   0.85  (-0.90) 0 1.0,
  GL.Vertex4 (-0.90)   0.85 0 1.0,
  GL.Vertex4   0.90  (-0.85) 0 1.0,
  GL.Vertex4   0.90    0.90 0 1.0,
  GL.Vertex4 (-0.85)   0.90 0 1.0]

colorsData :: [GL.Color4 Float]
colorsData = [
  GL.Color4   0.90  0.50  0.5   0.1,  -- Triangle 1
  GL.Color4   0.35  0.14  0.5   0.2,
  GL.Color4   0.50  0.62  0.5   0.6,
  GL.Color4   0.60  0.0   0.5   0.75,  -- Triangle 2
  GL.Color4   0.20  0.20  0.5   0.04,
  GL.Color4   0.15  0.49  0.5   0.23]

program = Program.createProgram "/Users/aravindhs/Aravindh/projects/haskell/fun/shaders/vertex.shader" "/Users/aravindhs/Aravindh/projects/haskell/fun/shaders/fragment.shader"


main :: IO ()
main = do
  win <- Window.createWindow 1920 1280 "Fun"
  program >>= useProgram
  let drawables = [toDrawable Red     $ Square (-0.5, -0.5) 1.0,
                toDrawable Green   $ Circle (0.5, 0.5) 0.5 100,
                toDrawable Blue    $ Rect (-1.0,0.33) (0.0,0.66),
                toDrawable White   $ Polyline [ (0.0,-0.66)
                                                ,(0.33,-0.33)
                                                ,(0.66,-0.66)
                                                ,(1.0,-0.33)]  0.01 
                ]
  
  drawIn win drawables
  Window.closeWindow win
  