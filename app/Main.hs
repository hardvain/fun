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
import Geometry
import Matrix 
import Time
import SceneGraph
import Renderable
import Color 
import Drawable 

main :: IO ()
main = do
  window <- createWindow 1920 1280 "Fun"
  startTime <- timeInMillis
  let square = Square (0,0,0) 0.5
  let circle = Circle (0.5, 0.5,0) 0.5 100
  let line = Polyline [ (0.0,-0.66,0) ,(0.33,-0.33,0) ,(0.66,-0.66,0) ,(1.0,-0.33,0)] 0.01
  color <- randomMaterialColor
  let squareDrawable = toDrawable square color
  let circleDrawable = toDrawable circle deepOrange400
  let lineDrawable = toDrawable line White 
  let squareRenderable = makeDefaultRenderable squareDrawable 
  let circleRenderable = makeDefaultRenderable circleDrawable 
  let lineRenderable = makeDefaultRenderable lineDrawable 
  let renderables = [ ]
  let sceneGraph = SceneGraph (Node lineRenderable renderables)
  let startingFrameNumber = 0
  draw sceneGraph window startingFrameNumber startTime
  closeWindow window
  
