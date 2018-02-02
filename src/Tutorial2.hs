module Tutorial2 where
import           Data.IORef
import           Graphics.UI.GLUT
import           Tutorial2.Bindings

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  _window <- createWindow "Hello World"
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just keyboardMouse
  angle <- newIORef 0.0
  displayCallback $= display angle
  idleCallback $= Just (idle angle)
  mainLoop
