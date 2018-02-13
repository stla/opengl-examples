module Mobius4
  where
import           Data.IORef
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Utils.Colour                 (interpolateColor)
import           Utils.Mobius                 (mobiusCurve', mobiusStrip''')
import           Utils.PrismaticPath          (prismaticPath')

white,black,gold :: Color4 GLfloat
white  = Color4 1    1    1    1
black  = Color4 0    0    0    1
gold   = Color4 1    0.84 0    1

vertexColor :: Vertex3 Double -> Color4 GLfloat
vertexColor v = interpolateColor (0,1,0) (0.5,0.5,0.5) (1,0,0) (zcoord v)
  where
    zcoord (Vertex3 _ _ z) = realToFrac ((z+0.25)/0.5)

display :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLdouble
        -> IORef GLdouble -> DisplayCallback
display rot1 rot2 rot3 zoom twists = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get rot1
  r2 <- get rot2
  r3 <- get rot3
  z <- get zoom
  k <- get twists
  let strip = mobiusStrip''' 180 1 0.25 k
      (curve1, curve2) = mobiusCurve' 200 1 0.25 k
      border1 = prismaticPath' curve1 30 0.025 False
      border2 = prismaticPath' curve2 30 0.025 False
  loadIdentity
  (_, size) <- get viewport
  resize z size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  renderPrimitive Triangles $ mapM_ drawTriangle strip
  renderPrimitive Quads $ do
    materialDiffuse Front $= gold
    mapM_ drawQuad' border1
    mapM_ drawQuad' border2
  swapBuffers
  where
    drawTriangle ((v1,v2,v3),n) = do
      normal n
      materialDiffuse Front $= vertexColor v1
      vertex v1
      materialDiffuse Front $= vertexColor v2
      vertex v2
      materialDiffuse Front $= vertexColor v3
      vertex v3
    drawQuad' ((v1,v2,v3,v4),n) = do
      normal n
      vertex v4
      vertex v3
      vertex v2
      vertex v1

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 (-2) (-2 + zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboardAndMouse :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat
                 -> IORef GLdouble -> IORef GLdouble -> KeyboardMouseCallback
keyboardAndMouse rot1 rot2 rot3 zoom twists key keyState _ _ =
  case (key, keyState) of
    (Char 'e', _)                   -> rot1 $~! subtract 1
    (Char 'r', _)                   -> rot1 $~! (+1)
    (Char 'd', _)                   -> rot2 $~! subtract 1
    (Char 'f', _)                   -> rot2 $~! (+1)
    (Char 'c', _)                   -> rot3 $~! subtract 1
    (Char 'v', _)                   -> rot3 $~! (+1)
    (Char 'o', Down)                -> twists $~! subtract 1
    (Char 'p', Down)                -> twists $~! (+1)
    (Char 'q', _)                   -> leaveMainLoop
    (MouseButton LeftButton, Down)  -> zoom $~! (+0.1)
    (MouseButton RightButton, Down) -> zoom $~! subtract 0.1
    _                               -> return ()

idle :: IdleCallback
idle = postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Möbius strip"
  windowSize $= Size 600 600
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= Color4 1 1 1 0
  lighting $= Enabled
  lightModelTwoSide $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 (-60) (-60) 0
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
--  specular (Light 0) $= black
  depthFunc $= Just Less
--  depthMask $= Enabled
  shadeModel $= Smooth
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  twists <- newIORef 1.0
  displayCallback $= display rot1 rot2 rot3 zoom twists
  reshapeCallback $= Just (resize 0)
  keyboardMouseCallback $= Just (keyboardAndMouse rot1 rot2 rot3 zoom twists)
  idleCallback $= Just idle
  mainLoop
