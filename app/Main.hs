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
import AST
import Data.Matrix 
program = Program.createProgram "/Users/aravindhs/Aravindh/projects/haskell/fun/shaders/vertex.vert" "/Users/aravindhs/Aravindh/projects/haskell/fun/shaders/fragment.frag"


initializeUniforms ::  IO [UniformData (GL.GLmatrix GL.GLfloat)]
initializeUniforms = do
  prog <- program
  transformLocation <- GL.uniformLocation (glProgram prog) "transform"
  transform <- GL.newMatrix GL.ColumnMajor (toList defaultMatrix) :: IO (GL.GLmatrix GL.GLfloat)
  return [UniformData "transform" transform transformLocation]

position :: Position
position = Position 0 0 0

rotation :: Rotation
rotation = Rotation 0 0 0

scale :: Scale
scale = Scale 0 0 0

transformation :: Transformation
transformation = Transformation position rotation scale

sceneGraph :: SceneGraph
sceneGraph = SceneGraph (Node (toDrawable (RGBA 0 0.5 0.5 0.8) (Square (-0.5, -0.5) 1.0)) [] (Just transformation))

main :: IO ()
main = do
  win <- Window.createWindow 1920 1280 "Fun"
  program >>= useProgram
  uniforms <- initializeUniforms
  startTime <- timeInMillis
  -- let drawables = [toDrawable (RGBA 0 0.5 0.5 0.8) (Square (-0.5, -0.5) 1.0) ,
  --               toDrawable (RGBA 0 0.5 0.5 0.6) (Circle (0.5, 0.5) 0.5 100),
  --               toDrawable (RGBA 1 0.5 0.5 0.1) (Circle (0.3, 0.3) 0.3 100),
  --               toDrawable Blue (Rect (-1.0,0.33) (0.0,0.66)),
  --               toDrawable White (Polyline [ (0.0,-0.66)
  --                                               ,(0.33,-0.33)
  --                                               ,(0.66,-0.66)
  --                                               ,(1.0,-0.33)] 0.01)
  --               ]

  draw sceneGraph win uniforms 0 startTime
  Window.closeWindow win
  
