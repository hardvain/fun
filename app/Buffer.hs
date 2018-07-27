module Buffer where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad
import System.Exit ( exitWith, ExitCode(..) )
import Shader
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Window

createBuffer :: (Storable a) => [a] -> BufferTarget  -> IO()
createBuffer datum bufferTarget = do
  buffer <- genObjectName
  bindBuffer bufferTarget $= Just buffer
  withArray datum $ \ptr -> do
    let size = fromIntegral ((length datum) * sizeOf (head datum))
    bufferData bufferTarget $= (size, ptr, StaticDraw)
