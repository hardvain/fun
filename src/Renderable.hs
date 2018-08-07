{-# LANGUAGE RankNTypes          #-}

module Renderable where

import qualified Matrix as M
import Data.Matrix
import Animation
import Ease
import Transformation 
import Drawable 
import Graphics.Rendering.OpenGL as GL
import SceneGraph

instance Show Drawable where
  show (Drawable vertices colors num) = "Vertices: " ++ (show vertices) ++ "\n" ++ "Colors: " ++ (show colors) ++ "\n" ++ "Number of Vertices: " ++ (show num)

data Renderable = Renderable {
  drawable:: Drawable,
  transformation :: Transformation,
  mvpMatrix ::  Matrix Float,
  animations :: [Animation]
}

makeDefaultRenderable :: Drawable -> Renderable
makeDefaultRenderable drawable = Renderable drawable defaultTransformation M.defaultMatrix [sampleAnimation RotationZ]

data RenderPipelineState a = RenderPipelineState {
  pipelineDescriptor :: RenderPipelineDescriptor,
  vertexBuffer :: [Vertex4 Float],
  fragmentBuffer :: [Vertex4 Float],
  uniform :: a,
  mode :: String
}

-- created when programs are created
data RenderPipelineDescriptor a = RenderPipelineDescriptor {
  vertexFunction :: String,
  fragmentFunction :: String,
  vertexDescriptor :: VertexDescriptor
} 

data VertexAttributeDescriptor = VertexAttributeDescriptor {
  dataType :: GL.DataType,
  offset :: Int,
  bufferIndex :: Int
}

data VertexLayoutDescriptor = VertexLayoutDescriptor {
  stride :: Int
}

data VertexDescriptor = VertexDescriptor {
  attributes :: [VertexAttributeDescriptor],
  layout :: VertexLayoutDescriptor
}

class Renderable r where
  render :: forall a. Int -> Int -> r -> RenderPipelineState a

data Square = Square {
  side :: Int
}
data Mesh = Mesh {
  renderable :: Renderable,
  vao :: String
}
renderSceneGraph :: SceneGraph Renderable -> IO ()
{-
  
-}