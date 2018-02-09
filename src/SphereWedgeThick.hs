module SphereWedgeThick
  where
import           Data.IORef
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Utils.SphereWedgeThick

white,black,green,red,blue :: Color4 GLfloat
white = Color4 1   1   1   1
black = Color4 0   0   0   1
green = Color4 0 (100/255) 0 1
red   = Color4 1   0   0   1
blue  = Color4 0   0   1   1

((wedge, wedge'), juncture) = sphereWedgeThick 0.3 0 (2*pi/3) 60 180

display :: IORef GLfloat -> IORef GLfloat -> IORef GLdouble
        -> DisplayCallback
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
    materialDiffuse Front $= red
    mapM_ drawTriangle (fst wedge)
  renderPrimitive Quads $ do
    materialDiffuse Front $= red
    mapM_ drawQuad (snd wedge)
  renderPrimitive Triangles $ do
    materialDiffuse Front $= green
    mapM_ drawTriangle (fst wedge')
  renderPrimitive Quads $ do
    materialDiffuse Front $= green
    mapM_ drawQuad (snd wedge')
  renderPrimitive Quads $ do
    materialDiffuse Front $= blue
    mapM_ drawQuad juncture
  swapBuffers
  where
    drawTriangle ((v1,v2,v3),n) = do
      normal n
      vertex v1
      vertex v2
      vertex v3
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
  lookAt (Vertex3 0 (-3) (-4 + zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboardAndMouse :: IORef GLfloat -> IORef GLfloat -> IORef GLdouble
                 -> KeyboardMouseCallback
keyboardAndMouse rot1 rot2 zoom key keyState _ _ =
  case (key, keyState) of
    (Char 'j', _)                   -> rot1 $~! subtract 1
    (Char 'k', _)                   -> rot1 $~! (+1)
    (Char 'h', _)                   -> rot2 $~! subtract 1
    (Char 'l', _)                   -> rot2 $~! (+1)
    (Char 'q', _)                   -> leaveMainLoop
    (MouseButton LeftButton, Down)  -> zoom $~! (+0.1)
    (MouseButton RightButton, Down) -> zoom $~! subtract 0.1
    _                               -> return ()

idle :: IdleCallback
idle = postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Thick sphere wedge"
  windowSize $= Size 600 600
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient Front $= black
  materialShininess Front $= 3
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 (-75) (-100) 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= black
  depthFunc $= Just Lequal
  depthMask $= Enabled
  shadeModel $= Flat
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  zoom <- newIORef 0.0
  displayCallback $= display rot1 rot2 zoom
  reshapeCallback $= Just (resize 0)
  keyboardMouseCallback $= Just (keyboardAndMouse rot1 rot2 zoom)
  idleCallback $= Just idle
  mainLoop
