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
import Matrix 
import Ease 
import Time

program = Program.createProgram "/Users/asridaran/Projects/haskell/fun/shaders/vertex.vert" "/Users/asridaran/Projects/haskell/fun/shaders/fragment.frag"


initializeUniforms ::  IO [UniformData]
initializeUniforms = do
  prog <- program
  translateLocation <- GL.uniformLocation (glProgram prog) "translate"
  rotateLocation <- GL.uniformLocation (glProgram prog) "rotate"
  return [UniformData "translate" (GL.Vector3 0.1 0.1 0.1 :: GL.Vector3 GL.GLfloat) translateLocation, UniformData "rotate" (GL.Vector3 0 0 3.14 :: GL.Vector3 GL.GLfloat) rotateLocation]


main :: IO ()
main = do
  win <- Window.createWindow 1920 1280 "Fun"
  program >>= useProgram
  uniforms <- initializeUniforms
  startTime <- timeInMillis
  let drawables = [toDrawable (RGBA 0 0.5 0.5 0.8) (Square (-0.5, -0.5) 1.0) ,
                toDrawable (RGBA 0 0.5 0.5 0.6) (Circle (0.5, 0.5) 0.5 100),
                toDrawable (RGBA 1 0.5 0.5 0.1) (Circle (0.3, 0.3) 0.3 100),
                toDrawable Blue (Rect (-1.0,0.33) (0.0,0.66)),
                toDrawable White (Polyline [ (0.0,-0.66)
                                                ,(0.33,-0.33)
                                                ,(0.66,-0.66)
                                                ,(1.0,-0.33)] 0.01)
                ]

  draw drawables win uniforms 0 startTime
  Window.closeWindow win
  