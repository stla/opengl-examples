module GreatIcosahedron
  where
import qualified Data.ByteString                   as B
import           Data.IORef
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           GreatIcosahedron.Data
import           Text.Printf
import           Utils.ConvertPPM
import           Utils.OpenGL                      (negateNormal,
                                                    triangleNormal)
import           Utils.Prism

grey1,grey9,blue,white,black,gold :: Color4 GLfloat
grey1 = Color4 0.1 0.1 0.1 1
grey9 = Color4 0.9 0.9 0.9 1
blue  = Color4 (25/255) (25/255) (112/255) 1
white = Color4 1   1   1   1
black = Color4 0   0   0   1
gold  = Color4 1 (215/255) 0 1

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
  mapM_ drawEdge edges
  renderPrimitive Triangles $ do
    materialDiffuse Front $= blue
    mapM_ drawTriangle polygons
  mapM_ drawVertex vertices
  swapBuffers

drawEdge :: (Vertex3 GLfloat, Vertex3 GLfloat) -> IO ()
drawEdge (v1,v2) = do
  let cylinder = prism v1 v2 30 0.025
  renderPrimitive Quads $ do
    materialDiffuse Front $= gold
    mapM_ drawQuad cylinder
  where
    drawQuad ((w1,w2,w3,w4),n) = do
      normal $ negateNormal n
      vertex w1
      vertex w2
      vertex w3
      vertex w4

drawTriangle :: [Vertex3 GLfloat] -> IO ()
drawTriangle vs = do
  normal $ triangleNormal (vs!!0, vs!!1, vs!!2)
  vertex (vs!!0)
  vertex (vs!!1)
  vertex (vs!!2)

drawVertex :: Vertex3 GLfloat -> IO ()
drawVertex v =
  preservingMatrix $ do
    translate $ toVector v
    materialDiffuse Front $= gold
    renderObject Solid $ Sphere' 0.05 30 30
  where
    toVector (Vertex3 x y z) = Vector3 x y z

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

idle :: IdleCallback
idle = postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Great icosahedron"
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= black
  materialAmbient Front $= black
  materialShininess Front $= 80
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
  zoom <- newIORef 0.0
  capture <- newIORef 1
  displayCallback $= display rot1 rot2 zoom
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 capture)
  mouseCallback $= Just (mouse zoom)
  idleCallback $= Just idle
  mainLoop
