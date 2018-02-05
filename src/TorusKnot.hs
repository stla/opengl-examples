module TorusKnot
  where
import           Data.IORef
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Utils.PrismaticPath          (prismaticPath')
import           Utils.TorusKnot

white,black,pink :: Color4 GLfloat
white = Color4 1   1   1   1
black = Color4 0   0   0   1
pink  = Color4 1 (192/255) (203/255) 1

display :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLint
        -> IORef GLint -> IORef GLdouble -> DisplayCallback
display rot1 rot2 rot3 p q zoom = do
  clear [ColorBuffer, DepthBuffer]
  a1 <- get rot1
  a2 <- get rot2
  a3 <- get rot3
  p' <- get p
  q' <- get q
  z <- get zoom
  let vertices = torusKnot (max 2 p') (max 2 q')
      knot = prismaticPath' vertices 30 0.15 True
  loadIdentity
  (_, size) <- get viewport
  resize z size
  rotate a1 $ Vector3 1 0 0
  rotate a2 $ Vector3 0 1 0
  rotate a3 $ Vector3 0 0 1
  renderPrimitive Quads $ do
    materialDiffuse Front $= pink
    mapM_ drawQuad knot
  swapBuffers
  where
    drawQuad ((v1,v2,v3,v4),n) = do
      normal n
      vertex v1
      vertex v2
      vertex v3
      vertex v4

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-12 + zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboardAndMouse :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat
                 -> IORef GLint -> IORef GLint -> IORef GLdouble
                 -> KeyboardMouseCallback
keyboardAndMouse rot1 rot2 rot3 p q zoom key keyState _ _ =
  case (key, keyState) of
    (Char 'e', _)                   -> rot1 $~! subtract 1
    (Char 'r', _)                   -> rot1 $~! (+1)
    (Char 'd', _)                   -> rot2 $~! subtract 1
    (Char 'f', _)                   -> rot2 $~! (+1)
    (Char 'c', _)                   -> rot3 $~! subtract 1
    (Char 'v', _)                   -> rot3 $~! (+1)
    (Char 'o', _)                   -> p $~! subtract 1
    (Char 'p', _)                   -> p $~! (+1)
    (Char 'l', _)                   -> q $~! subtract 1
    (Char 'm', _)                   -> q $~! (+1)
    (Char 'q', _)                   -> leaveMainLoop
    (MouseButton LeftButton, Down)  -> zoom $~! (+0.1)
    (MouseButton RightButton, Down) -> zoom $~! subtract 0.1
    _                               -> return ()

idle :: IdleCallback
idle = postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Torus knot"
  windowSize $= Size 600 600
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= black
  materialAmbient Front $= black
  materialShininess Front $= 30
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
  ambient (Light 0) $= white
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  depthFunc $= Just Lequal
  depthMask $= Enabled
  shadeModel $= Smooth
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  p <- newIORef 2
  q <- newIORef 2
  displayCallback $= display rot1 rot2 rot3 p q zoom
  reshapeCallback $= Just (resize 0)
  keyboardMouseCallback $= Just (keyboardAndMouse rot1 rot2 rot3 p q zoom)
  idleCallback $= Just idle
  mainLoop
