module Buffer where

import Graphics.Rendering.OpenGL as GL
import Foreign.Marshal.Array
import Foreign.Storable
import Window
import Foreign.Ptr

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

type Positions = [GL.Vertex3 GL.GLfloat]

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

getBufferSize :: (Storable a) => [a] -> GLsizeiptr
getBufferSize datum = fromIntegral ((length datum) * sizeOf (head datum))

describeAttribute :: Int -> Int -> GL.DataType -> IO AttribLocation
describeAttribute attributeIndex dimension dataType = do
  let attribLocation = GL.AttribLocation (fromIntegral attributeIndex)
  GL.vertexAttribPointer attribLocation GL.$=
    (GL.ToFloat, GL.VertexArrayDescriptor (fromIntegral dimension) dataType 0 (bufferOffset 0))
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

