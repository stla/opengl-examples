module Strange
  where
import           Data.IORef
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Strange.Data

white,black :: Color4 GLfloat
white = Color4 1   1   1   1
black = Color4 0   0   0   1

display :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLdouble
        -> DisplayCallback
display rot1 rot2 rot3 zoom = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get rot1
  r2 <- get rot2
  r3 <- get rot3
  z <- get zoom
  loadIdentity
  (_, size) <- get viewport
  resize z size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  renderPrimitive Triangles $
    mapM_ drawTriangle strange
  swapBuffers
  where
    drawTriangle ((cv1,cv2,cv3),n) = do
      normal n
      materialDiffuse Front $= snd cv1
      vertex (fst cv1)
      materialDiffuse Front $= snd cv2
      vertex (fst cv2)
      materialDiffuse Front $= snd cv3
      vertex (fst cv3)

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 (4 - zoom) 0) (Vertex3 0 0 0) (Vector3 0 0 1)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboardAndMouse :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat
                 -> IORef GLdouble -> KeyboardMouseCallback
keyboardAndMouse rot1 rot2 rot3 zoom key keyState _ _ =
  case (key, keyState) of
    (Char 'e', _)                   -> rot1 $~! subtract 1
    (Char 'r', _)                   -> rot1 $~! (+1)
    (Char 'd', _)                   -> rot2 $~! subtract 1
    (Char 'f', _)                   -> rot2 $~! (+1)
    (Char 'c', _)                   -> rot3 $~! subtract 1
    (Char 'v', _)                   -> rot3 $~! (+1)
    (Char 'q', _)                   -> leaveMainLoop
    (MouseButton LeftButton, Down)  -> zoom $~! (+0.1)
    (MouseButton RightButton, Down) -> zoom $~! subtract 0.1
    _                               -> return ()

idle :: IdleCallback
idle = postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Strange hull"
  windowSize $= Size 600 600
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient Front $= black
  materialShininess Front $= 3
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 100 0 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= black
  depthFunc $= Just Lequal
  depthMask $= Enabled
  shadeModel $= Flat
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  displayCallback $= display rot1 rot2 rot3 zoom
  reshapeCallback $= Just (resize 0)
  keyboardMouseCallback $= Just (keyboardAndMouse rot1 rot2 rot3 zoom)
  idleCallback $= Just idle
  mainLoop
