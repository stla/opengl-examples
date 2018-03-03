module SineSurface
  where
import           Control.Monad                     (when)
import qualified Data.ByteString                   as B
import           Data.IORef
import           Data.List
import           Data.Tuple.Extra                  (both)
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           SineSurface.Data
import           Text.Printf
import           Utils.OpenGL

grey1,grey9,purple,white,black,gold :: Color4 GLfloat
grey1  = Color4 0.1 0.1 0.1 1
grey9  = Color4 0.9 0.9 0.9 1
purple = Color4 0.5 0   0.5 1
white  = Color4 1   1   1   1
black  = Color4 0   0   0   1
gold   = Color4 1 (215/255) 0 1

ssurface :: [[Double]]
ssurface = sinesurface 3

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
  rotate a $ Vector3 1 1 (1::GLdouble)
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  mapM_ (\vec -> preservingMatrix $ do
                  translate (toVec vec)
                  materialDiffuse Front $= purple
                  renderObject Solid $ Sphere' 0.2 50 50)
        ssurface
  swapBuffers
  where
    toVec x = Vector3 (x!!0) (x!!1) (x!!2)

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

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLint
         -> IORef Bool -> KeyboardCallback
keyboard rot1 rot2 rot3 capture anim c _ =
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
    'a' -> writeIORef anim True
    'q' -> leaveMainLoop
    _   -> return ()

mouse :: IORef GLdouble -> MouseCallback
mouse zoom button keyState _ =
  case (button, keyState) of
    (LeftButton, Down)  -> zoom $~! (+0.1)
    (RightButton, Down) -> zoom $~! subtract 0.1
    _                   -> return ()

idle :: IORef Bool -> IORef GLdouble -> IdleCallback
idle anim angle = do
  a <- get anim
  r <- get angle
  when a $ do
    when (r < 360) $ do
      let ppm = printf "sinesurface%04d.ppm" (round r :: Int)
      (>>=) capturePPM (B.writeFile ppm)
    angle $~! (+ 1)
  postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Sine Surface"
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
  anim <- newIORef False
  angle <- newIORef 0.0
  displayCallback $= display rot1 rot2 rot3 zoom angle
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 capture anim)
  mouseCallback $= Just (mouse zoom)
  idleCallback $= Just (idle anim angle)
  mainLoop
