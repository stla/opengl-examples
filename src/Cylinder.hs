module Cylinder
  where
import qualified Data.ByteString                   as B
import           Data.IORef
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Text.Printf
import           Utils.ConvertPPM
import           Utils.Cylinder
import           Utils.OpenGL                      (triangleNormal, vertex3f)

white,black,blue :: Color4 GLfloat
white = Color4 1   1   1   1
black = Color4 0   0   0   1
blue  = Color4 0   0   1   1

v1' :: Vertex3 GLfloat
v1' = Vertex3 (-2) (-2) (-2)

v2' :: Vertex3 GLfloat
v2' = Vertex3 2 2 2

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
  lighting $= Disabled
  renderPrimitive TriangleStrip $ drawCylinder v1' v2' 0.25
  lighting $= Enabled
  preservingMatrix $ do
    translate vec1'
    materialDiffuse Front $= blue
    renderObject Solid $ Sphere' 0.5 50 50
  preservingMatrix $ do
    translate vec2'
    materialDiffuse Front $= blue
    renderObject Solid $ Sphere' 0.5 50 50
  swapBuffers
  where
    vec1' = vertex3toVector3 v1'
    vec2' = vertex3toVector3 v2'
    vertex3toVector3 (Vertex3 x y z) = Vector3 x y z

drawCylinder :: Vertex3 GLfloat -> Vertex3 GLfloat -> GLfloat -> IO ()
drawCylinder v1 v2 radius = do
  let vs = tstripCylinder v1 v2 radius
  -- materialDiffuse FrontAndBack $= blue
  color $ Color3 0 0 (1 :: GLfloat)
  -- normal (triangleNormal (vs!!0, vs!!1, vs!!2))
  mapM_ vertex3f vs

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-11 + zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
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
      let ppm = printf "cylinder%04d.ppm" i
          png = printf "cylinder%04d.png" i
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
  _ <- createWindow "Cylinder"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= black
  materialAmbient Front $= black
  materialShininess Front $= 80
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
  capture <- newIORef 1
  displayCallback $= display rot1 rot2 zoom
  reshapeCallback $= Just (resize 0)
  keyboardMouseCallback $= Just (keyboardAndMouse rot1 rot2 capture zoom)
  idleCallback $= Just idle
  mainLoop
