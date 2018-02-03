module Prism
  where
import           Data.IORef
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Utils.Prism
import           Utils.OpenGL                      (vertex3f)

white,black,blue :: Color4 GLfloat
white = Color4 1   1   1   1
black = Color4 0   0   0   1
blue  = Color4 0   0   1   1

v1',v2',w1',w2' :: Vertex3 GLfloat
v1' = Vertex3 (-2) (-2) (-2)
v2' = Vertex3 2 2 2
w1' = Vertex3 4 0 0
w2' = Vertex3 5 4 1

display :: IORef GLfloat -> IORef GLfloat -> IORef GLdouble -> DisplayCallback
display rot1 rot2 zoom = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get rot1
  r2 <- get rot2
  z <- get zoom
  let prism6 = prism v1' v2' 6 1
      prism100 = prism w1' w2' 100 1
  loadIdentity
  (_, size) <- get viewport
  resize z size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  mapM_ (renderPrimitive Quads . drawPrismFacet) prism6
  mapM_ (renderPrimitive Quads . drawPrismFacet) prism100
  swapBuffers
  where
    drawPrismFacet ((v1,v2,v3,v4),n) = do
      materialDiffuse FrontAndBack $= blue
      normal n
      vertex v1
      vertex v2
      vertex v3
      vertex v4
      -- where
      --   negaten (Normal3 a b c) =
      --     let h = sqrt (a*a + b*b + c*c) in Normal3 (a/h) (b/h) (c/h)

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-16 + zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboardAndMouse :: IORef GLfloat -> IORef GLfloat -> IORef GLdouble
                 -> KeyboardMouseCallback
keyboardAndMouse rot1 rot2 zoom key keyState _ _ =
  case (key, keyState) of
    (Char 'j', _) -> rot1 $~! subtract 1
    (Char 'k', _) -> rot1 $~! (+1)
    (Char 'h', _) -> rot2 $~! subtract 1
    (Char 'l', _) -> rot2 $~! (+1)
    (Char 'q', _) -> leaveMainLoop
    (MouseButton LeftButton, Down)  -> zoom $~! (+0.1)
    (MouseButton RightButton, Down) -> zoom $~! subtract 0.1
    _   -> return ()

idle :: IdleCallback
idle = postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Prism"
  windowSize $= Size 600 600
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= black
  materialAmbient Front $= black
  materialShininess Front $= 30
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
  lightModelTwoSide $= Enabled
  ambient (Light 0) $= white
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  depthFunc $= Just Lequal
  depthMask $= Enabled
  shadeModel $= Smooth
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  zoom <- newIORef 0.0
  displayCallback $= display rot1 rot2 zoom
  reshapeCallback $= Just (resize 0)
  keyboardMouseCallback $= Just (keyboardAndMouse rot1 rot2 zoom)
  idleCallback $= Just idle
  mainLoop
