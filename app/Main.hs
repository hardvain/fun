module Main where
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Control.Monad
import System.Exit ( exitWith, ExitCode(..) )
import OpenGL.Shader
import Foreign.Marshal.Array
import Foreign.Ptr
import OpenGL.Window
import Foreign.Storable
import OpenGL.Buffer 
import OpenGL.Program
import Renderer
import Shape
import Matrix 
import Ease 
import Time
import AST
import Data.Matrix
import SceneGraph
import Renderable
import Color 

main :: IO ()
main = do
  window <- createWindow 1920 1280 "Fun"
  startTime <- timeInMillis
  let square = toDrawable (hex 0x673ab7) (Square (-0.5, -0.5) 1.0)
  let circle = toDrawable (hex 0x673ab7) (Circle (0.5, 0.5) 0.5 100)
  let polyLine =  toDrawable White (Polyline [ (0.0,-0.66) ,(0.33,-0.33) ,(0.66,-0.66) ,(1.0,-0.33)] 0.01)
  let squareRenderable = makeDefaultRenderable square 
  let circleRenderable = makeDefaultRenderable circle 
  let renderables = [ makeNode circleRenderable
                    , makeNode $ makeDefaultRenderable polyLine
                    ]
  let sceneGraph = SceneGraph (Node squareRenderable renderables)
  let startingFrameNumber = 0
  -- let drawables = [toDrawable (RGBA 0 0.5 0.5 0.8) (Square (-0.5, -0.5) 1.0) ,
  --               toDrawable (RGBA 0 0.5 0.5 0.6) (Circle (0.5, 0.5) 0.5 100),
  --               toDrawable (RGBA 1 0.5 0.5 0.1) (Circle (0.3, 0.3) 0.3 100),
  --               toDrawable Blue (Rect (-1.0,0.33) (0.0,0.66)),
  --               toDrawable White (Polyline [ (0.0,-0.66)
  --                                               ,(0.33,-0.33)
  --                                               ,(0.66,-0.66)
  --                                               ,(1.0,-0.33)] 0.01)
  --               ]

  draw sceneGraph window startingFrameNumber startTime
  closeWindow window
  
