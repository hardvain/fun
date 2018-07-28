module Buffer where

import Graphics.Rendering.OpenGL as GL
import Foreign.Marshal.Array
import Foreign.Storable
import Window

createArrayBuffer :: (Storable a) => [a] -> IO()
createArrayBuffer = createBuffer  ArrayBuffer

createBuffer :: (Storable a) => BufferTarget -> [a] -> IO()
createBuffer bufferTarget datum = do
  buffer <- genObjectName
  bindBuffer bufferTarget $= Just buffer
  withArray datum $ \ptr -> do
    let size = fromIntegral ((length datum) * sizeOf (head datum))
    bufferData bufferTarget $= (size, ptr, StaticDraw)

createVertexArrayObject :: IO VertexArrayObject
createVertexArrayObject = genObjectName

withVertexObject :: VertexArrayObject -> IO () -> IO()
withVertexObject vertexObject action = do
  bindVertexArrayObject $= Just vertexObject
  action
  bindVertexArrayObject $= Nothing

