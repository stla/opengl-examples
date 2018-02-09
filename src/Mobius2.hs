module Mobius2
  where
import           Data.IORef
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Utils.Mobius                 (mobiusStrip', mobiusCurve')
import           Utils.PrismaticPath          (prismaticPath')
import           Utils.Colour                 (interpolateColor)

white,black,orange :: Color4 GLfloat
white  = Color4 1    1    1    1
black  = Color4 0    0    0    1
orange = Color4 1    0.55 0    1

strip :: [((Vertex3 GLdouble, Vertex3 GLdouble, Vertex3 GLdouble,
            Vertex3 GLdouble), Normal3 Double)]
strip = mobiusStrip' 200 1 0.25 5

curve :: [Vertex3 GLdouble]
curve = mobiusCurve' 200 1 0.25 5

border :: [((Vertex3 GLdouble, Vertex3 GLdouble, Vertex3 GLdouble,
             Vertex3 GLdouble), Normal3 GLdouble)]
border = prismaticPath' curve 30 0.025 True

vertexColor :: Vertex3 Double -> Color4 GLfloat
vertexColor v = interpolateColor (0,0,1) (1,1,0) (1,0,0) (zcoord v)
  where
    zcoord (Vertex3 _ _ z) = realToFrac ((z+0.25)/0.5)

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
  rotate (60 :: GLfloat) $ Vector3 1 0 0
  rotate (90 :: GLfloat) $ Vector3 0 0 1
  rotate (180 :: GLfloat) $ Vector3 0 1 0
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  renderPrimitive Quads $ mapM_ drawQuad strip
  renderPrimitive Quads $ do
    materialDiffuse Front $= orange
    mapM_ drawQuad' border
  swapBuffers
  where
    drawQuad ((v1,v2,v3,v4),n) = do
      normal n
      materialDiffuse FrontAndBack $= vertexColor v1
      vertex v1
      materialDiffuse FrontAndBack $= vertexColor v2
      vertex v2
      materialDiffuse FrontAndBack $= vertexColor v3
      vertex v3
      materialDiffuse FrontAndBack $= vertexColor v4
      vertex v4
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
  lookAt (Vertex3 0 0 (-3 + zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
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
  _ <- createWindow "Möbius strip"
  windowSize $= Size 600 600
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  -- cullFace $= Just Back
  clearColor $= white
  materialAmbient FrontAndBack $= Color4 0.5 0.5 0.5 1
  materialShininess Front $= 10
  lighting $= Enabled
  lightModelTwoSide $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 50 (-100) 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= black
  depthFunc $= Just Less
  depthMask $= Enabled
  shadeModel $= Smooth
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  displayCallback $= display rot1 rot2 rot3 zoom
  reshapeCallback $= Just (resize 0)
  keyboardMouseCallback $= Just (keyboardAndMouse rot1 rot2 rot3 zoom)
  idleCallback $= Just idle
  mainLoop
