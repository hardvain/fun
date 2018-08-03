module Buffer where

import Graphics.Rendering.OpenGL as GL
import Foreign.Marshal.Array
import Foreign.Storable
import Window
import Foreign.Ptr

data VertexAttributeDescriptor = VertexAttributeDescriptor {
  attributeLocation :: Int,
  dimension :: Int
}

createAndDescribeBuffer :: (Storable a) => [a] -> Int -> Int -> IO GL.BufferObject
createAndDescribeBuffer datum location dimension = do
  bufferObject <- createArrayBuffer datum
  let descriptor = VertexAttributeDescriptor location dimension
  describeAttribute descriptor
  return bufferObject

createArrayBuffer :: (Storable a) => [a] -> IO GL.BufferObject
createArrayBuffer = createBuffer  ArrayBuffer

createBuffer :: (Storable a) => BufferTarget -> [a] -> IO GL.BufferObject
createBuffer bufferTarget datum = do
  buffer <- genObjectName
  bindBuffer bufferTarget $= Just buffer
  withArray datum $ \ptr -> do
    let size = getBufferSize datum
    bufferData bufferTarget $= (size, ptr, StaticDraw)
  return buffer

type Vertices = [GL.Vertex4 Float]

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

getBufferSize :: (Storable a) => [a] -> GLsizeiptr
getBufferSize datum = fromIntegral ((length datum) * sizeOf (head datum))


describeAttribute :: VertexAttributeDescriptor -> IO AttribLocation
describeAttribute (VertexAttributeDescriptor attributeIndex dimension) = do
  let attribLocation = GL.AttribLocation (fromIntegral attributeIndex)
  GL.vertexAttribPointer attribLocation GL.$=
    (GL.ToFloat, GL.VertexArrayDescriptor (fromIntegral dimension) GL.Float 0 (bufferOffset 0))
  vertexAttribArray attribLocation $= Enabled
  return attribLocation

createVertexArrayObject :: IO VertexArrayObject
createVertexArrayObject = genObjectName

withVertexArrayObject :: VertexArrayObject -> IO a -> IO a
withVertexArrayObject vertexObject action = do
  bindVertexArrayObject $= Just vertexObject
  result <- action
  bindVertexArrayObject $= Nothing
  return result


withNewVertexArrayObject :: (VertexArrayObject -> IO a) -> IO a
withNewVertexArrayObject f = do
  vao <- createVertexArrayObject
  withVertexArrayObject vao $ f vao

  