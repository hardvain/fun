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
import Time
import SceneGraph
import Renderable
import Color 

main :: IO ()
main = do
  window <- createWindow 1920 1280 "Fun"
  startTime <- timeInMillis
  let squarePoints = Square (0,0,0) 0.5
  let square = toDrawable red500 squarePoints
  let circle = toDrawable blue500 (Circle (0.5, 0.5,0) 0.5 100)
  let polyLine =  toDrawable White (Polyline [ (0.0,-0.66,0) ,(0.33,-0.33,0) ,(0.66,-0.66,0) ,(1.0,-0.33,0)] 0.01)
  let squareRenderable = makeDefaultRenderable square 
  let circleRenderable = makeDefaultRenderable circle 
  let renderables = [ ]
  let sceneGraph = SceneGraph (Node squareRenderable renderables)
  let startingFrameNumber = 0
  draw sceneGraph window startingFrameNumber startTime
  closeWindow window
  
