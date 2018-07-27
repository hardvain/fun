module Program (
  createProgram,
  useProgram
) where
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad
import System.Exit ( exitWith, ExitCode(..) )
import Shader
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

createProgram :: String -> String -> IO GL.Program
createProgram vertexShaderPath fragmentShaderPath = loadShaders [
     ShaderInfo GL.VertexShader (FileSource vertexShaderPath),
     ShaderInfo GL.FragmentShader (FileSource fragmentShaderPath)]

useProgram :: GL.Program -> IO()
useProgram program = do
  GL.currentProgram GL.$= Just program