module Main where
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
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


position :: Position
position = Position 0 0 0

rotation :: Rotation
rotation = Rotation 0 0 0

scale :: Scale
scale = Scale 0 0 0

transformation :: Transformation
transformation = Transformation position rotation scale

defaultPosition :: Position
defaultPosition = Position 0 0 0

defaultRotation :: Rotation
defaultRotation = Rotation 0 0 0

defaultScale :: Scale
defaultScale = Scale 1 1 1

defaultTransformation :: Transformation
defaultTransformation = Transformation defaultPosition defaultRotation defaultScale

makeDefaultRenderable :: Drawable -> Renderable
makeDefaultRenderable drawable = Renderable drawable defaultTransformation defaultMatrix

makeSceneGraph :: a -> SceneGraph a
makeSceneGraph a = SceneGraph (makeNode a)

main :: IO ()
main = do
  window <- createWindow 1920 1280 "Fun"
  startTime <- timeInMillis
  let square = toDrawable (RGBA 0 0.5 0.5 0.8) (Square (-0.5, -0.5) 1.0)
  let circle = toDrawable (RGBA 0 0.5 0.5 0.6) (Circle (0.5, 0.5) 0.5 100)
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
  
