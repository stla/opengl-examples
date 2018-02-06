module SphereMesh
  where
import           Data.IORef
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Utils.SphereMesh

white,black,green :: Color4 GLfloat
white = Color4 1   1   1   1
black = Color4 0   0   0   1
green = Color4 0 (100/255) 0 1

display :: IORef GLfloat -> IORef GLfloat -> IORef GLdouble -> IORef Int
        -> DisplayCallback
display rot1 rot2 zoom iter = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get rot1
  r2 <- get rot2
  z <- get zoom
  i <- get iter
  let mesh = sphereMesh (max 0 i)
  loadIdentity
  (_, size) <- get viewport
  resize z size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  preservingMatrix $ do
    scale 1 0.5 (0.5 :: GLdouble)
    renderPrimitive Triangles $ do
      materialDiffuse Front $= green
      mapM_ drawTriangle mesh
  swapBuffers
  where
    drawTriangle ((v1,v2,v3),n) = do
      normal n
      vertex v1
      vertex v2
      vertex v3

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-3 + zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboardAndMouse :: IORef GLfloat -> IORef GLfloat -> IORef GLdouble
                 -> IORef Int -> KeyboardMouseCallback
keyboardAndMouse rot1 rot2 zoom iter key keyState _ _ =
  case (key, keyState) of
    (Char 'j', _)                   -> rot1 $~! subtract 1
    (Char 'k', _)                   -> rot1 $~! (+1)
    (Char 'h', _)                   -> rot2 $~! subtract 1
    (Char 'l', _)                   -> rot2 $~! (+1)
    (Char 'o', _)                   -> iter $~! subtract 1
    (Char 'p', _)                   -> iter $~! (+1)
    (Char 'q', _)                   -> leaveMainLoop
    (MouseButton LeftButton, Down)  -> zoom $~! (+0.1)
    (MouseButton RightButton, Down) -> zoom $~! subtract 0.1
    _                               -> return ()

idle :: IdleCallback
idle = postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Sphere mesh"
  windowSize $= Size 600 600
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient Front $= Color4 0.5 0.5 0.5 1
  materialShininess Front $= 3
--  colorMaterial $= Just (Front, Specular)
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 (-50) 0 (-100) 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= black
  depthFunc $= Just Lequal
  depthMask $= Enabled
  shadeModel $= Flat
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  zoom <- newIORef 0.0
  iter <- newIORef 0
  displayCallback $= display rot1 rot2 zoom iter
  reshapeCallback $= Just (resize 0)
  keyboardMouseCallback $= Just (keyboardAndMouse rot1 rot2 zoom iter)
  idleCallback $= Just idle
  mainLoop
