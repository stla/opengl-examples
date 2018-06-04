module Cyclide.Cyclide where
import           Control.Monad                     (when)
import           Cyclide.Data
import qualified Data.ByteString                   as B
import           Data.IORef
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Text.Printf

data Context = Context
    {
      contextRot1      :: IORef GLfloat
    , contextRot2      :: IORef GLfloat
    , contextRot3      :: IORef GLfloat
    , contextTriangles :: IORef [NTriangle]
    }

white,black,red :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1
red        = Color4    1    0    0    1

cyclide :: Double -> [NTriangle]
cyclide = allTriangles 400 3.1 3.0

display :: Context -> IORef GLdouble -> IORef GLfloat -> DisplayCallback
display context zoom alpha = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  triangles <- get (contextTriangles context)
  z <- get zoom
  alpha' <- get alpha
  loadIdentity
  (_, size) <- get viewport
  resize z size
  rotate alpha' $ Vector3 0 1 0
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  renderPrimitive Triangles $
    mapM_ drawTriangle triangles
  swapBuffers
  where
    drawTriangle ((v1,v2,v3),norm) = do
      materialDiffuse FrontAndBack $= red
      normal norm
      vertex v1
      vertex v2
      vertex v3

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-6+zoom)) (Vertex3 0 1 0) (Vector3 0 (-1) 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h


keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef GLdouble -- zoom
         -> IORef Bool -- animation
         -> IORef Double -- d
         -> IORef [NTriangle]
         -> KeyboardCallback
keyboard rot1 rot2 rot3 zoom anim d triangles c _ = do
  case c of
    'e' -> rot1 $~! subtract 2
    'r' -> rot1 $~! (+2)
    't' -> rot2 $~! subtract 2
    'y' -> rot2 $~! (+2)
    'u' -> rot3 $~! subtract 2
    'i' -> rot3 $~! (+2)
    'm' -> zoom $~! (+1)
    'l' -> zoom $~! subtract 1
    'a' -> writeIORef anim True
    'h' -> do
      d $~! (+0.1)
      d' <- get d
      writeIORef triangles (cyclide d')
    'n' -> do
      d $~! (\x -> if x >= 0.1 then x-0.1 else x)
      d' <- get d
      writeIORef triangles (cyclide d')
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing


idle :: IORef Bool -> IORef Int -> IORef GLfloat -> IdleCallback
idle anim snapshots alpha = do
    a <- get anim
    s <- get snapshots
    when a $ do
      when (s < 360) $ do
        let ppm = printf "ppm/cyclide%04d.ppm" s
        (>>=) capturePPM (B.writeFile ppm)
      snapshots $~! (+1)
      alpha $~! (+1)
      postRedisplay Nothing
    return ()

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Dupin cyclide"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient FrontAndBack $= black
  materialShininess FrontAndBack $= 5
  materialSpecular Front $= white
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
  lightModelTwoSide $= Enabled
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  depthFunc $= Just Less
  shadeModel $= Smooth
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  anim <- newIORef False
  alpha <- newIORef 0.0
  snapshots <- newIORef 0
  let d = 1.3
  d' <- newIORef d
  cyclide' <- newIORef (cyclide d)
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextTriangles = cyclide'}
                             zoom alpha
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 zoom anim d' cyclide')
  idleCallback $= Just (idle anim snapshots alpha)
  putStrLn "*** Dupin cyclide ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Increase/decrease parameter: h,n\n\
        \    Animation: a\n\
        \"
  mainLoop
