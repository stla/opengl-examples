module NonconvexPolyhedron
  where
import qualified Data.ByteString                   as B
import           Data.IORef
import           Data.List.Index                   (imapM_)
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           NonconvexPolyhedron.Data2
import           Text.Printf
import           Utils.Colour
import           Utils.ConvertPPM
import           Utils.OpenGL                      (triangleNormal)

grey1,grey9,purple,white,black,gold :: Color4 GLfloat
grey1  = Color4 0.1 0.1 0.1 1
grey9  = Color4 0.9 0.9 0.9 1
purple = Color4 0.5 0   0.5 1
white  = Color4 1   1   1   1
black  = Color4 0   0   0   1
gold   = Color4 1 (215/255) 0 1

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
  imapM_ drawTetrahedron tetrahedraFaces''
  -- mapM_ drawEdge edges
  -- renderPrimitive Triangles $ do
  --   materialDiffuse Front $= purple
  --   mapM_ drawTriangle polygons
  -- mapM_ drawVertex vertices
  swapBuffers

drawTetrahedron :: Int -> [[Vertex3 GLdouble]] -> IO ()
drawTetrahedron i face =
  renderPrimitive Triangles $ do
    materialDiffuse Front $= pickColor i
    mapM_ drawTriangle face
    where
      drawTriangle vs = do
        normal $ triangleNormal (vs!!0, vs!!1, vs!!2)
        vertex (vs!!0)
        vertex (vs!!1)
        vertex (vs!!2)

-- drawEdge :: (Vertex3 GLfloat, Vertex3 GLfloat) -> IO ()
-- drawEdge (v1,v2) = do
--   let cylinder = prism v1 v2 30 0.025
--   renderPrimitive Quads $ do
--     materialDiffuse Front $= gold
--     mapM_ drawQuad cylinder
--   where
--     drawQuad ((w1,w2,w3,w4),n) = do
--       normal $ negateNormal n
--       vertex w1
--       vertex w2
--       vertex w3
--       vertex w4
--
-- drawTriangle :: [Vertex3 GLfloat] -> IO ()
-- drawTriangle vs = do
--   normal $ triangleNormal (vs!!0, vs!!1, vs!!2)
--   vertex (vs!!0)
--   vertex (vs!!1)
--   vertex (vs!!2)
--
-- drawVertex :: Vertex3 GLfloat -> IO ()
-- drawVertex v =
--   preservingMatrix $ do
--     translate $ toVector v
--     materialDiffuse Front $= gold
--     renderObject Solid $ Sphere' 0.05 30 30
--   where
--     toVector (Vertex3 x y z) = Vector3 x y z

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-10 + zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLint
         -> KeyboardCallback
keyboard rot1 rot2 rot3 capture c _ =
  case c of
    'r' -> rot1 $~! subtract 1
    't' -> rot1 $~! (+1)
    'f' -> rot2 $~! subtract 1
    'g' -> rot2 $~! (+1)
    'v' -> rot3 $~! subtract 1
    'b' -> rot3 $~! (+1)
    'c' -> do
      i <- get capture
      let ppm = printf "pic%04d.ppm" i
          png = printf "pic%04d.png" i
      (>>=) capturePPM (B.writeFile ppm)
      convert ppm png True
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
  _ <- createWindow "Nonconvex Polyhedron"
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient Front $= black
  materialShininess Front $= 80
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  -- specular (Light 0) $= white
  depthFunc $= Just Lequal
  depthMask $= Enabled
  shadeModel $= Smooth
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  capture <- newIORef 1
  displayCallback $= display rot1 rot2 rot3 zoom
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 capture)
  mouseCallback $= Just (mouse zoom)
  idleCallback $= Just idle
  mainLoop
