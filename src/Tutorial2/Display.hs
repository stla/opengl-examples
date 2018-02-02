module Tutorial2.Display (display, idle) where
import           Control.Monad
import           Data.IORef
import           Graphics.UI.GLUT
import           Tutorial2.Cube
import           Tutorial2.Points

display :: IORef GLfloat -> DisplayCallback
display angle = do
  clear [ColorBuffer]
  loadIdentity
  a <- get angle
  rotate a $ Vector3 0 0 1
  scale 0.7 0.7 (0.7::GLfloat)
  forM_ (points 7) $ \(x,y,z) ->
    preservingMatrix $ do
      color $ Color3 ((x+1)/2) ((y+1)/2) ((z+1)/2)
      translate $ Vector3 x y z
      cube 0.1
  swapBuffers

idle :: IORef GLfloat -> IdleCallback
idle angle = do
  angle $~! (+ 0.1)
  postRedisplay Nothing
