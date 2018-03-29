module ConicalSpiral.ConicalSpiral where
import           ConicalSpiral.Data
import           Control.Monad                     (when)
import qualified Data.ByteString                   as B
import           Data.IORef
-- import           Data.List
-- import           Data.List.Index                   (imap, imapM_)
-- import           Data.Tuple.Extra                  (both)
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Text.Printf
-- import           Utils.Colour
import           Utils.Quads.Color

white,black,grey,whitesmoke,red :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1
grey       = Color4  0.8  0.8  0.8  0.7
whitesmoke = Color4 0.96 0.96 0.96    1
red        = Color4    1    0    0    1


display :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat
        -> IORef GLdouble -> IORef GLdouble -> IORef GLdouble -> IORef GLdouble
        -> IORef GLdouble -> DisplayCallback
display rot1 rot2 rot3 alpha beta gamma n zoom = do
  clear [ColorBuffer, DepthBuffer]
  alpha' <- get alpha
  beta' <- get beta
  gamma' <- get gamma
  n' <- get n
  r1 <- get rot1
  r2 <- get rot2
  r3 <- get rot3
  z <- get zoom
  let surface = allQuads 300 alpha' beta' gamma' n'
  loadIdentity
  (_, size) <- get viewport
  resize z size
--  rotate alpha $ Vector3 1 1 (1::GLdouble)
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  renderPrimitive Quads $
--    materialDiffuse FrontAndBack $= red
    mapM_ drawQuad surface
  swapBuffers
  where
    drawQuad ((v1,v2,v3,v4),norm) = do
      materialDiffuse FrontAndBack $= quadColor (v1,v2,v3,v4) Nothing
      normal norm
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
  lookAt (Vertex3 0 0 (-15+zoom)) (Vertex3 0 2 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h


keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat
         -> IORef GLdouble -> IORef GLdouble -> IORef GLdouble -> IORef GLdouble
         -> IORef GLdouble -> IORef Bool -> KeyboardCallback
keyboard rot1 rot2 rot3 alpha beta gamma n zoom anim c _ =
  case c of
    'c' -> alpha $~! subtract 0.1
    'd' -> alpha $~! (+ 0.1)
    'v' -> beta $~! subtract 0.1
    'f' -> beta $~! (+ 0.1)
    'b' -> gamma $~! subtract 0.1
    'g' -> gamma $~! (+ 0.1)
    'n' -> n $~! subtract 0.1
    'h' -> n $~! (+ 0.1)
    'e' -> rot1 $~! subtract 1
    'r' -> rot1 $~! (+1)
    't' -> rot2 $~! subtract 1
    'y' -> rot2 $~! (+1)
    'u' -> rot3 $~! subtract 1
    'i' -> rot3 $~! (+1)
    'm' -> zoom $~! (+1)
    'l' -> zoom $~! subtract 1
    'a' -> writeIORef anim True
    'q' -> leaveMainLoop
    _   -> return ()


idle :: IORef Bool -> IORef GLdouble -> IORef Int -> IdleCallback
idle anim n snapshots = do
    a <- get anim
    s <- get snapshots
    when a $ do
      when (s < 360) $ do
        let ppm = printf "ppm/conicalspiral%04d.ppm" s
        (>>=) capturePPM (B.writeFile ppm)
      n $~! (+ 0.2)
      snapshots $~! (+1)
    postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Conical Spiral"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient FrontAndBack $= black
  materialShininess FrontAndBack $= 95
  materialSpecular Front $= white
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
  lightModelTwoSide $= Enabled
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  depthFunc $= Just Less
  depthMask $= Enabled
  shadeModel $= Smooth
  blend $= Enabled    -- allow transparency
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  alpha <- newIORef 1.0
  beta <- newIORef 8.0
  gamma <- newIORef 0.2
  n <- newIORef 5.0
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  anim <- newIORef False
  snapshots <- newIORef 0
  displayCallback $= display rot1 rot2 rot3 alpha beta gamma n zoom
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 alpha beta gamma n zoom anim)
  idleCallback $= Just (idle anim n snapshots)
  putStrLn "*** Haskell OpenGL Conical Spiral ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Parameters: d, f, g, h, c, b, v, n \n\
        \    Animation: a\n\
        \"
  mainLoop

