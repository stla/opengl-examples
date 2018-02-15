module TriangulatedSphere
  where
import           Data.IORef
import           Data.List.Index              (imapM_)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           TriangulatedSphere.Data
import           Utils.Colour
import           Utils.OpenGL                 (negateNormal)
import           Utils.SphericalTriangle

white,black :: Color4 GLfloat
white  = Color4 1   1   1   1
black  = Color4 0   0   0   1

striangles :: [[((Vertex3 Double, Vertex3 Double, Vertex3 Double), Normal3 Double)]]
striangles = map (\t -> stMesh 6 (t!!2) (t!!1) (t!!0)) triangles

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
  imapM_ drawSTriangle striangles
  swapBuffers

drawSTriangle :: Int -> [((Vertex3 Double, Vertex3 Double, Vertex3 Double), Normal3 Double)] -> IO ()
drawSTriangle i striangle =
  renderPrimitive Triangles $ do
    materialDiffuse Front $= pickColor i
    mapM_ drawTriangle striangle

drawTriangle :: ((Vertex3 Double, Vertex3 Double, Vertex3 Double), Normal3 Double) -> IO ()
drawTriangle ((v1, v2, v3), n) = do
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

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat
         -> KeyboardCallback
keyboard rot1 rot2 rot3 c _ =
  case c of
    'r' -> rot1 $~! subtract 1
    't' -> rot1 $~! (+1)
    'f' -> rot2 $~! subtract 1
    'g' -> rot2 $~! (+1)
    'v' -> rot3 $~! subtract 1
    'b' -> rot3 $~! (+1)
    'q' -> leaveMainLoop
    _   -> return ()

mouse :: IORef GLdouble -> MouseCallback
mouse zoom button keyState _ =
  case (button, keyState) of
    (LeftButton, Down)  -> zoom $~! (+0.1)
    (RightButton, Down) -> zoom $~! subtract 0.1
    _                   -> return ()

idle :: IdleCallback
idle = postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Spherical Triangle"
  windowSize $= Size 600 600
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient Front $= black
  lighting $= Enabled
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
  keyboardCallback $= Just (keyboard rot1 rot2 rot3)
  mouseCallback $= Just (mouse zoom)
  idleCallback $= Just idle
  mainLoop
