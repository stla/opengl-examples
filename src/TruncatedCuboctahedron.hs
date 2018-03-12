module TruncatedCuboctahedron
  where
import qualified Data.ByteString                   as B
import           Data.IORef
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Text.Printf
import           TruncatedCuboctahedron.Data       (edges, polygons)
import           Utils.ConvertPPM
import           Utils.OpenGL                      (triangleNormal, vertex3f)

-- axes directions:
-- x: left
-- y: up
-- z: back

grey1,grey9,red,white,black :: Color4 GLfloat
grey1 = Color4 0.1 0.1 0.1 1
grey9 = Color4 0.9 0.9 0.9 1
red   = Color4 1   0   0   1
white = Color4 1   1   1   1
black = Color4 0   0   0   1

display :: IORef GLfloat -> IORef GLfloat -> IORef GLdouble -> DisplayCallback
display rot1 rot2 zoom = do
  clear [ColorBuffer, DepthBuffer]
  lineWidth $= 2
  r1 <- get rot1
  r2 <- get rot2
  z <- get zoom
  loadIdentity
  (_, size) <- get viewport
  resize z size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  lighting $= Disabled
  mapM_ (renderPrimitive Lines . drawEdge) edges
  lighting $= Enabled
  mapM_ (renderPrimitive Polygon . drawPolygon) polygons
  swapBuffers

drawEdge :: (Vertex3 GLfloat, Vertex3 GLfloat) -> IO ()
drawEdge (v1, v2) = do
  color (Color3 0 0 0 :: Color3 GLfloat)
  vertex3f v1
  vertex3f v2

drawPolygon :: [Vertex3 GLfloat] -> IO ()
drawPolygon xs = do
  materialDiffuse FrontAndBack $= red
  normal (triangleNormal (xs!!0, xs!!1, xs!!2))
  mapM_ vertex3f xs

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-15 + zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLint -> KeyboardCallback
keyboard rot1 rot2 capture c _ =
  case c of
    'j' -> rot1 $~! subtract 1
    'k' -> rot1 $~! (+1)
    'h' -> rot2 $~! subtract 1
    'l' -> rot2 $~! (+1)
    'c' -> do
      i <- get capture
      (>>=) capturePPM (B.writeFile (printf "pic%04d.ppm" i))
      convert (printf "pic%04d.ppm" i) (printf "pic%04d.png" i) True
      capture $~! (+1)
    'q' -> leaveMainLoop
    _   -> return ()

mouse :: IORef GLdouble -> MouseCallback
mouse zoom button keyState _ =
  case (button, keyState) of
    (LeftButton, Down)  -> zoom $~! (+0.1)
    (RightButton, Down) -> zoom $~! subtract 0.1
    _                   -> return ()

motion :: MotionCallback -- print cursor position
motion = print

idle :: IdleCallback
idle = postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Truncated Cuboctahedron"
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= black
  materialAmbient FrontAndBack $= black
  materialShininess FrontAndBack $= 100
  materialSpecular Front $= white
  materialShininess Front $= 100.0
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
  blend $= Enabled                         -- allow transparency (not used here)
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  zoom <- newIORef 0.0
  capture <- newIORef 1
  displayCallback $= display rot1 rot2 zoom
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 capture)
  mouseCallback $= Just (mouse zoom)
  motionCallback $= Just motion
  idleCallback $= Just idle
  mainLoop
