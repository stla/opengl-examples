module Hopf.HopfTorus
  where
import           Data.IORef
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Hopf.HopfCircle
import           Utils.PrismaticPath          (prismaticPath')

white,black,green :: Color4 GLfloat
white = Color4 1   1   1   1
black = Color4 0   0   0   1
green = Color4 0   1   0   1

theta_ :: [Double]
theta_ = [2*pi * frac i 400 | i <- [0..400]]
  where
  frac :: Int -> Int -> Double
  frac p q = realToFrac p / realToFrac q

phi_ :: [Double]
phi_ = map (\x -> 3*pi/4*sin(3*x)) theta_

vertices :: [[Vertex3 Double]]
vertices = zipWith vCircle phi_ theta_

circles :: [[((Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double), Normal3 Double)]]
circles = map (\x -> prismaticPath' x 30 0.2 True) vertices

display :: IORef GLfloat -> IORef GLfloat -> IORef GLdouble
        -> DisplayCallback
display rot1 rot2 zoom = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get rot1
  r2 <- get rot2
  z <- get zoom
  loadIdentity
  (_, size) <- get viewport
  resize z size
  rotate r1 $ Vector3 1 1 1
  rotate r2 $ Vector3 0 1 0
  mapM_ (\circle -> renderPrimitive Quads $ do
                      materialDiffuse Front $= green
                      mapM_ drawQuad circle) circles
  swapBuffers
  where
    drawQuad ((v1,v2,v3,v4),n) = do
      normal n
      vertex v1
      vertex v2
      vertex v3
      vertex v4

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-17 + zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboardAndMouse :: IORef GLfloat -> IORef GLfloat -> IORef GLdouble
                 -> KeyboardMouseCallback
keyboardAndMouse rot1 rot2 zoom key keyState _ _ =
  case (key, keyState) of
    (Char 'j', _)                   -> rot1 $~! subtract 1
    (Char 'k', _)                   -> rot1 $~! (+1)
    (Char 'h', _)                   -> rot2 $~! subtract 1
    (Char 'l', _)                   -> rot2 $~! (+1)
    (Char 'q', _)                   -> leaveMainLoop
    (MouseButton LeftButton, Down)  -> zoom $~! (+0.5)
    (MouseButton RightButton, Down) -> zoom $~! subtract 0.5
    _                               -> return ()

idle :: IdleCallback
idle = postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Hopf Torus"
  windowSize $= Size 600 600
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient Front $= Color4 0.5 0.5 0.5 1
  materialShininess Front $= 30
  lighting $= Enabled
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
  zoom <- newIORef 0.0
  displayCallback $= display rot1 rot2 zoom
  reshapeCallback $= Just (resize 0)
  keyboardMouseCallback $= Just (keyboardAndMouse rot1 rot2 zoom)
  idleCallback $= Just idle
  mainLoop
