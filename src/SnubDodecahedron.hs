module SnubDodecahedron where
import SnubDodecahedron.Data
import Data.List.Index (imapM_)
import           Data.IORef
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import Utils.OpenGL
import Utils.Colour
import           Utils.Prism


white,black,grey,whitesmoke,red,green :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1
grey       = Color4  0.8  0.8  0.8  0.7
whitesmoke = Color4 0.96 0.96 0.96    1
red = Color4 1 0 0 1
green = Color4 0 1 0 1

display :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLdouble
        -> IORef GLdouble -> DisplayCallback
display rot1 rot2 rot3 zoom angle = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get rot1
  r2 <- get rot2
  r3 <- get rot3
  z <- get zoom
  a <- get angle
  loadIdentity
  (_, size) <- get viewport
  resize z size
  rotate (180::GLfloat) $ Vector3 0 1 0
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  mapM_ drawPolygonR facesIdxsR
  mapM_ drawPolygonG facesIdxsG
  mapM_ drawEdges edges
  swapBuffers

drawPolygonR :: [Vertex3 GLfloat] -> IO ()
drawPolygonR vs =
  renderPrimitive Polygon $ do
  materialDiffuse Front $= red
  normal $ triangleNormal (vs!!0, vs!!1, vs!!2)
  vertex (vs!!0)
  vertex (vs!!1)
  vertex (vs!!2)

drawPolygonG :: [Vertex3 GLfloat] -> IO ()
drawPolygonG vs =
  renderPrimitive Polygon $ do
  materialDiffuse Front $= green
  normal $ triangleNormal (vs!!0, vs!!1, vs!!2)
  vertex (vs!!0)
  vertex (vs!!1)
  vertex (vs!!2)

drawEdges :: (Vertex3 GLfloat, Vertex3 GLfloat) -> IO ()
drawEdges (v1, v2) = do
  let cylinder = prism v1 v2 30 0.4
  renderPrimitive Quads $ do
    materialDiffuse Back $= whitesmoke
    mapM_ f cylinder
  where
    f ((w1,w2,w3,w4),n) = do
      normal n
      vertex w1
      vertex w2
      vertex w3
      vertex w4


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

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLdouble
         -> KeyboardCallback
keyboard rot1 rot2 rot3 angle c _ =
  case c of
    'r' -> rot1 $~! subtract 1
    't' -> rot1 $~! (+1)
    'f' -> rot2 $~! subtract 1
    'g' -> rot2 $~! (+1)
    'v' -> rot3 $~! subtract 1
    'b' -> rot3 $~! (+1)
    'o' -> angle $~! subtract 0.1
    'p' -> angle $~! (+0.1)
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
  -- materialShininess Front $= 80
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
  ambient (Light 0) $= white
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  depthFunc $= Just Less
  depthMask $= Enabled
  shadeModel $= Smooth
  blend $= Enabled    -- allow transparency
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  angle <- newIORef 0.0
  displayCallback $= display rot1 rot2 rot3 zoom angle
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 angle)
  mouseCallback $= Just (mouse zoom)
  idleCallback $= Just idle
  mainLoop
