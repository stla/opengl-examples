module RegularSphere
  where
import           Data.List
import qualified Data.ByteString                   as B
import           Data.Tuple.Extra                  (both)
import           Data.IORef
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Utils.RegularSphere.RegularSphere (regularSphere)
import           Text.Printf
import Utils.OpenGL

grey1,grey9,purple,white,black,gold :: Color4 GLfloat
grey1  = Color4 0.1 0.1 0.1 1
grey9  = Color4 0.9 0.9 0.9 1
purple = Color4 0.5 0   0.5 1
white  = Color4 1   1   1   1
black  = Color4 0   0   0   1
gold   = Color4 1 (215/255) 0 1

sphere :: ([[Double]], [[(Int,Int)]])
sphere@(vs,rectIdxs) = regularSphere 40 [0,0,0] 1 -- rectangles (transpose): [[(0,1),(0,2),(0,3)], [(1,1),(1,2),(1,3)], [(2,1),(2,2),(2,3)], [(3,1),(3,2),(3,3)]]
--rectIdxs' = transpose rectIdxs
-- s2 = snd sphere
-- rect@(r1,r2,r3,r4) = s2 !! 0 !! 3 -- 4 vertices indices
-- rect' = [(s1 !! r1), (s1 !! r2), (s1 !! r3), (s1 !! r4)] -- 4 vertices

rectangles :: [Vertex3 Double]
rectangles = map toVx3 (concat rectIdxs)
  where
    toVx3 i = (Vertex3 (vs!!(max ((snd i)-1) 0)!!0) (vs!!(max ((snd i)-1) 0)!!1) (vs!!(max ((snd i)-1) 0)!!2))

rectangles2 :: [Vertex3 Double]
rectangles2 = map toVx3 (concat rectIdxs)
  where
    toVx3 i = (Vertex3 (vs!!(max ((snd i)-1) 1)!!0) (vs!!(max ((snd i)-1) 1)!!1) (vs!!(max ((snd i)-1) 1)!!2))

-- rectangles3 :: [Vertex3 Double]
-- rectangles3 = map toVx3 (concat rectIdxs)
--   where
--     toVx3 i = (Vertex3 (vs!!(max ((snd i)-1) 0)!!3) (vs!!(max ((snd i)-1) 0)!!4) (vs!!(max ((snd i)-1) 0)!!5))

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
  renderPrimitive Quads $ do
    materialDiffuse Front $= purple
    drawQuads rectangles
    drawQuads rectangles2
--    drawQuads rectangles3
  swapBuffers
  where
    drawQuads :: [Vertex3 Double] -> IO ()
    drawQuads vert = do
         normal $ negateNormal $ triangleNormal (vert!!0, vert!!1, vert!!2)
         mapM_ vertex vert

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
      (>>=) capturePPM (B.writeFile ppm)
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
  _ <- createWindow "Great stellated dodecahedron"
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient Front $= black
  materialShininess Front $= 80
  lighting $= Enabled
--  lightModelTwoSide $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= black
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
