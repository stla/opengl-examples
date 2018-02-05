module Mobius
  where
import           Data.IORef
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Utils.Mobius

white,black,blue :: Color4 GLfloat
white = Color4 1   1   1   1
black = Color4 0   0   0   1
blue  = Color4 0   0   1   1

strip :: [((Vertex3 GLdouble, Vertex3 GLdouble, Vertex3 GLdouble, Vertex3 GLdouble), Normal3 Double)]
strip = mobiusStrip (1/2) 1

curve :: [Vertex3 GLdouble]
curve = mobiusCurve (1/2) 1

display :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLdouble
        -> DisplayCallback
display rot1 rot2 rot3 zoom = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get rot1
  r2 <- get rot2
  r3 <- get rot3
  z <- get zoom
  loadIdentity
--  rotate (-40::GLfloat) $ Vector3 1 0 0
  (_, size) <- get viewport
  resize z size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  pointSize $= 10
  -- lighting $= Disabled
  -- drawCurve curve
  -- lighting $= Enabled
  drawStrip strip
  mapM_ (\v -> preservingMatrix $ do
               translate $ toVector3 v
               materialDiffuse Front $= Color4 1 0 0 1
               renderObject Solid $ Sphere' 0.05 50 50)
        curve
  swapBuffers
  where
    toVector3 (Vertex3 x y z) = Vector3 x y z
    drawStrip = mapM_ (renderPrimitive Quads . drawStripFacet)
      where
        drawStripFacet ((v1,v2,v3,v4),n) = do
          materialDiffuse FrontAndBack $= blue
          normal n
          vertex v1
          vertex v2
          vertex v3
          vertex v4
    -- drawCurve x = renderPrimitive Points $ do
    --   color (Color3 1 0 0 :: Color3 GLfloat)
    --   mapM_ vertex x


resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-5 + zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboardAndMouse :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat
                 -> IORef GLdouble -> KeyboardMouseCallback
keyboardAndMouse rot1 rot2 rot3 zoom key keyState _ _ =
  case (key, keyState) of
    (Char 'e', _) -> rot1 $~! subtract 1
    (Char 'r', _) -> rot1 $~! (+1)
    (Char 'd', _) -> rot2 $~! subtract 1
    (Char 'f', _) -> rot2 $~! (+1)
    (Char 'c', _) -> rot3 $~! subtract 1
    (Char 'v', _) -> rot3 $~! (+1)
    (Char 'q', _) -> leaveMainLoop
    (MouseButton LeftButton, Down)  -> zoom $~! (+0.1)
    (MouseButton RightButton, Down) -> zoom $~! subtract 0.1
    _   -> return ()

idle :: IdleCallback
idle = postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Mobius strip"
  windowSize $= Size 600 600
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
--  cullFace $= Just Back
  clearColor $= white
--  materialAmbient FrontAndBack $= Color4 0.5 0.5 0.5 1
--  materialShininess Front $= 30
  lighting $= Enabled
  lightModelTwoSide $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
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
