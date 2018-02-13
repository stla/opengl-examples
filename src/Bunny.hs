module Bunny
  where
import qualified Data.ByteString                   as B
import           Data.IORef
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Bunny.Data
import           Text.Printf
import           Utils.ConvertPPM
import           Utils.OpenGL                      (triangleNormal)

white,black,sienna :: Color4 GLfloat
white  = Color4 1    1    1    1
black  = Color4 0    0    0    1
sienna = Color4 0.63 0.32 0.18 1

display :: IORef GLfloat -> IORef GLfloat -> IORef GLdouble -> DisplayCallback
display rot1 rot2 zoom = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get rot1
  r2 <- get rot2
  z <- get zoom
  loadIdentity
  (_, size) <- get viewport
  resize z size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  renderPrimitive Triangles $ do
    materialDiffuse Front $= sienna
    mapM_ drawTriangle triangles
  swapBuffers

drawTriangle :: [Vertex3 GLfloat] -> IO ()
drawTriangle xs = do
  normal (triangleNormal (xs!!0, xs!!1, xs!!2))
  mapM_ vertex xs

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 0.1 100.0
  lookAt (Vertex3 0 0.1 (0.3 + zoom)) (Vertex3 0 0.1 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboardAndMouse :: IORef GLfloat -> IORef GLfloat -> IORef GLint
                 -> IORef GLdouble -> KeyboardMouseCallback
keyboardAndMouse rot1 rot2 capture zoom key keyState _ _ =
  case (key, keyState) of
    (Char 'j', _) -> rot1 $~! subtract 1
    (Char 'k', _) -> rot1 $~! (+1)
    (Char 'h', _) -> rot2 $~! subtract 1
    (Char 'l', _) -> rot2 $~! (+1)
    (Char 'c', _) -> do
      i <- get capture
      let ppm = printf "bunny%04d.ppm" i
          png = printf "bunny%04d.png" i
      (>>=) capturePPM (B.writeFile ppm)
      convert ppm png True
      capture $~! (+1)
    (Char 'q', _) -> leaveMainLoop
    (MouseButton LeftButton, Down)  -> zoom $~! (+0.1)
    (MouseButton RightButton, Down) -> zoom $~! subtract 0.1
    _   -> return ()

idle :: IdleCallback
idle = postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Bunny"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient Front $= black
  materialShininess Front $= 10
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 10 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= black
  depthFunc $= Just Lequal
  depthMask $= Enabled
  shadeModel $= Smooth
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  zoom <- newIORef 0.0
  capture <- newIORef 1
  displayCallback $= display rot1 rot2 zoom
  reshapeCallback $= Just (resize 0)
  keyboardMouseCallback $= Just (keyboardAndMouse rot1 rot2 capture zoom)
  idleCallback $= Just idle
  mainLoop
