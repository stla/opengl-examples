module TeapotAnimation
  where
import           Control.Monad                     (when)
import qualified Data.ByteString                   as B
import           Data.IORef
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Teapot.Data                       (triangles)
import           Text.Printf
import           Utils.ConvertPPM
import           Utils.OpenGL                      (triangleNormal, vertex3f)

red,white,chrome :: Color4 GLfloat
red    = Color4 1   0   0   1
white  = Color4 1   1   1   1
chrome = Color4 (199/255) (196/255) (185/255) 1

display :: IORef GLfloat -> IORef GLint -> IORef GLfloat -> IORef GLfloat
        -> IORef GLfloat -> DisplayCallback
display angle capture rot1 rot2 rot3 = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get rot1
  r2 <- get rot2
  r3 <- get rot3
  a <- get angle
  i <- get capture
  loadIdentity
  rotate (-30 :: GLfloat) $ Vector3 1 0 0
  rotate a $ Vector3 0 1 0
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  mapM_ (renderPrimitive Triangles . drawTriangle) triangles
  when (i > 0) $ do
    (>>=) capturePPM (B.writeFile (printf "teapot%04d.ppm" i))
    convert (printf "teapot%04d.ppm" i) (printf "teapot%04d.png" i) True
    capture $~! (+1)
  swapBuffers

drawTriangle :: (Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat) -> IO ()
drawTriangle vs@(v1,v2,v3) = do
  materialDiffuse FrontAndBack $= chrome
  normal (triangleNormal vs)
  vertex3f v1
  vertex3f v2
  vertex3f v3

resize :: Size -> IO ()
resize s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-10)) (Vertex3 0 1.5 0) (Vector3 0 1 0)
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
    'c' -> capture $~! (+1)
    'q' -> leaveMainLoop
    _   -> return ()

idle :: IORef GLfloat -> IdleCallback
idle angle = do
  angle $~! (+ 3)
  postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Teapot"
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= Color4 0 0 0 0
  materialAmbient FrontAndBack $= Color4 0 0 0 0
  materialShininess FrontAndBack $= 50
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
  rot3 <- newIORef 0.0
  capture <- newIORef 0
  angle <- newIORef 0.0
  displayCallback $= display angle capture rot1 rot2 rot3
  reshapeCallback $= Just resize
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 capture)
  idleCallback $= Just (idle angle)
  mainLoop
