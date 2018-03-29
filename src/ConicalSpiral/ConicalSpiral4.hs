module ConicalSpiral.ConicalSpiral4 where
import           ConicalSpiral.Data4
import           Data.IORef
import qualified Data.Map.Strict              as M
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT

white,black :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1

colors :: [Color4 GLfloat]
colors =
  [ Color4 0 0 0.02 1
  , Color4 0 0 0.02 1
  , Color4 0 0 0.03 1
  , Color4 0.01 0 0.04 1
  , Color4 0.01 0.01 0.04 1
  , Color4 0.01 0.01 0.05 1
  , Color4 0.01 0.01 0.07 1
  , Color4 0.02 0.02 0.08 1
  , Color4 0.02 0.02 0.09 1
  , Color4 0.02 0.02 0.1 1
  , Color4 0.03 0.02 0.11 1
  , Color4 0.03 0.03 0.12 1
  , Color4 0.04 0.03 0.13 1
  , Color4 0.04 0.04 0.14 1
  , Color4 0.05 0.04 0.15 1
  , Color4 0.05 0.04 0.16 1
  , Color4 0.06 0.04 0.17 1
  , Color4 0.07 0.05 0.18 1
  , Color4 0.07 0.05 0.19 1
  , Color4 0.07 0.05 0.21 1
  , Color4 0.08 0.05 0.22 1
  , Color4 0.09 0.06 0.23 1
  , Color4 0.09 0.06 0.24 1
  , Color4 0.1 0.06 0.25 1
  , Color4 0.11 0.06 0.26 1
  , Color4 0.11 0.07 0.28 1
  , Color4 0.12 0.07 0.29 1
  , Color4 0.13 0.07 0.3 1
  , Color4 0.13 0.07 0.31 1
  , Color4 0.14 0.07 0.33 1
  , Color4 0.15 0.07 0.34 1
  , Color4 0.16 0.07 0.35 1
  , Color4 0.16 0.07 0.36 1
  , Color4 0.17 0.07 0.38 1
  , Color4 0.18 0.07 0.38 1
  , Color4 0.19 0.07 0.4 1
  , Color4 0.2 0.06 0.4 1
  , Color4 0.21 0.06 0.42 1
  , Color4 0.22 0.06 0.42 1
  , Color4 0.22 0.06 0.43 1
  , Color4 0.24 0.06 0.44 1
  , Color4 0.24 0.06 0.44 1
  , Color4 0.25 0.06 0.45 1
  , Color4 0.26 0.06 0.46 1
  , Color4 0.27 0.06 0.46 1
  , Color4 0.27 0.06 0.47 1
  , Color4 0.29 0.06 0.47 1
  , Color4 0.29 0.06 0.47 1
  , Color4 0.3 0.07 0.48 1
  , Color4 0.31 0.07 0.48 1
  , Color4 0.32 0.07 0.49 1
  , Color4 0.33 0.07 0.49 1
  , Color4 0.33 0.08 0.49 1
  , Color4 0.34 0.08 0.49 1
  , Color4 0.35 0.08 0.49 1
  , Color4 0.36 0.09 0.49 1
  , Color4 0.36 0.09 0.5 1
  , Color4 0.37 0.09 0.5 1
  , Color4 0.38 0.09 0.5 1
  , Color4 0.39 0.1 0.5 1
  , Color4 0.4 0.1 0.5 1
  , Color4 0.4 0.11 0.5 1
  , Color4 0.41 0.11 0.51 1
  , Color4 0.42 0.11 0.51 1
  , Color4 0.43 0.11 0.51 1
  , Color4 0.43 0.12 0.51 1
  , Color4 0.44 0.12 0.51 1
  , Color4 0.45 0.13 0.51 1
  , Color4 0.46 0.13 0.51 1
  , Color4 0.47 0.13 0.51 1
  , Color4 0.47 0.13 0.51 1
  , Color4 0.48 0.14 0.51 1
  , Color4 0.49 0.14 0.51 1
  , Color4 0.5 0.15 0.51 1
  , Color4 0.51 0.15 0.51 1
  , Color4 0.51 0.15 0.51 1
  , Color4 0.52 0.15 0.51 1
  , Color4 0.53 0.15 0.51 1
  , Color4 0.54 0.16 0.51 1
  , Color4 0.55 0.16 0.51 1
  , Color4 0.55 0.16 0.51 1
  , Color4 0.56 0.16 0.51 1
  , Color4 0.57 0.17 0.51 1
  , Color4 0.58 0.17 0.5 1
  , Color4 0.58 0.17 0.5 1
  , Color4 0.6 0.18 0.5 1
  , Color4 0.6 0.18 0.5 1
  , Color4 0.61 0.18 0.5 1
  , Color4 0.62 0.18 0.5 1
  , Color4 0.63 0.18 0.5 1
  , Color4 0.64 0.19 0.49 1
  , Color4 0.64 0.19 0.49 1
  , Color4 0.65 0.19 0.49 1
  , Color4 0.66 0.2 0.49 1
  , Color4 0.67 0.2 0.49 1
  , Color4 0.68 0.2 0.49 1
  , Color4 0.68 0.2 0.48 1
  , Color4 0.69 0.21 0.48 1
  , Color4 0.7 0.21 0.48 1
  , Color4 0.71 0.21 0.48 1
  , Color4 0.72 0.22 0.47 1
  , Color4 0.73 0.22 0.47 1
  , Color4 0.74 0.22 0.47 1
  , Color4 0.74 0.22 0.47 1
  , Color4 0.75 0.23 0.47 1
  , Color4 0.76 0.23 0.46 1
  , Color4 0.77 0.24 0.46 1
  , Color4 0.77 0.24 0.45 1
  , Color4 0.78 0.24 0.45 1
  , Color4 0.79 0.24 0.45 1
  , Color4 0.8 0.25 0.44 1
  , Color4 0.8 0.25 0.44 1
  , Color4 0.82 0.25 0.44 1
  , Color4 0.82 0.26 0.44 1
  , Color4 0.83 0.26 0.43 1
  , Color4 0.84 0.27 0.43 1
  , Color4 0.84 0.27 0.42 1
  , Color4 0.85 0.27 0.42 1
  , Color4 0.86 0.28 0.42 1
  , Color4 0.87 0.29 0.41 1
  , Color4 0.87 0.29 0.41 1
  , Color4 0.88 0.3 0.4 1
  , Color4 0.89 0.3 0.4 1
  , Color4 0.89 0.31 0.39 1
  , Color4 0.9 0.31 0.39 1
  , Color4 0.91 0.32 0.39 1
  , Color4 0.91 0.33 0.38 1
  , Color4 0.92 0.34 0.38 1
  , Color4 0.92 0.34 0.38 1
  , Color4 0.93 0.35 0.38 1
  , Color4 0.93 0.36 0.37 1
  , Color4 0.94 0.36 0.37 1
  , Color4 0.94 0.37 0.37 1
  , Color4 0.95 0.38 0.36 1
  , Color4 0.95 0.39 0.36 1
  , Color4 0.95 0.4 0.36 1
  , Color4 0.96 0.41 0.36 1
  , Color4 0.96 0.42 0.36 1
  , Color4 0.96 0.42 0.36 1
  , Color4 0.96 0.43 0.36 1
  , Color4 0.97 0.44 0.36 1
  , Color4 0.97 0.45 0.36 1
  , Color4 0.97 0.46 0.36 1
  , Color4 0.98 0.47 0.36 1
  , Color4 0.98 0.48 0.36 1
  , Color4 0.98 0.49 0.37 1
  , Color4 0.98 0.5 0.37 1
  , Color4 0.98 0.51 0.37 1
  , Color4 0.98 0.52 0.38 1
  , Color4 0.98 0.53 0.38 1
  , Color4 0.99 0.54 0.38 1
  , Color4 0.99 0.55 0.39 1
  , Color4 0.99 0.56 0.39 1
  , Color4 0.99 0.56 0.4 1
  , Color4 0.99 0.58 0.4 1
  , Color4 0.99 0.58 0.41 1
  , Color4 0.99 0.6 0.41 1
  , Color4 0.99 0.6 0.42 1
  , Color4 0.99 0.61 0.42 1
  , Color4 1 0.62 0.43 1
  , Color4 1 0.63 0.43 1
  , Color4 1 0.64 0.44 1
  , Color4 1 0.65 0.45 1
  , Color4 1 0.66 0.45 1
  , Color4 1 0.67 0.45 1
  , Color4 1 0.68 0.47 1
  , Color4 1 0.69 0.47 1
  , Color4 1 0.7 0.48 1
  , Color4 1 0.71 0.48 1
  , Color4 1 0.72 0.49 1
  , Color4 1 0.73 0.5 1
  , Color4 1 0.73 0.51 1
  , Color4 1 0.75 0.51 1
  , Color4 1 0.75 0.52 1
  , Color4 1 0.76 0.53 1
  , Color4 1 0.77 0.53 1
  , Color4 1 0.78 0.55 1
  , Color4 1 0.79 0.55 1
  , Color4 1 0.8 0.56 1
  , Color4 1 0.81 0.57 1
  , Color4 1 0.82 0.58 1
  , Color4 1 0.83 0.58 1
  , Color4 1 0.84 0.6 1
  , Color4 1 0.84 0.6 1
  , Color4 0.99 0.85 0.61 1
  , Color4 0.99 0.86 0.62 1
  , Color4 0.99 0.87 0.63 1
  , Color4 0.99 0.88 0.64 1
  , Color4 0.99 0.89 0.65 1
  , Color4 0.99 0.9 0.65 1
  , Color4 0.99 0.91 0.66 1
  , Color4 0.99 0.92 0.67 1
  , Color4 0.99 0.93 0.68 1
  , Color4 0.99 0.94 0.69 1
  , Color4 0.99 0.95 0.7 1
  , Color4 0.99 0.96 0.71 1
  , Color4 0.99 0.96 0.72 1
  , Color4 0.99 0.97 0.73 1
  , Color4 0.99 0.98 0.74 1
  , Color4 0.99 0.99 0.75 1
  ]

display :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
        -> IORef GLdouble  -- parameter n
        -> IORef GLdouble  -- zoom
        -> DisplayCallback
display rot1 rot2 rot3 n zoom = do
  clear [ColorBuffer, DepthBuffer]
  n' <- get n
  r1 <- get rot1
  r2 <- get rot2
  r3 <- get rot3
  z <- get zoom
  (_, size) <- get viewport
  let surface = allQuads 200 200 n'
  loadIdentity
  resize z size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  renderPrimitive Quads $ mapM_ drawQuad (M.toList surface)
  swapBuffers
  where
    drawQuad ((_,j), quad) = do
      materialDiffuse FrontAndBack $= colors !! j
      drawQuad' quad
        where
          drawQuad' ((v1,v2,v3,v4),norm) = do
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

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef GLdouble -- parameter n
         -> IORef GLdouble -- zoom
         -> KeyboardCallback
keyboard rot1 rot2 rot3 n zoom c _ =
  case c of
    'b' -> n $~! subtract 0.1
    'n' -> n $~! (+ 0.1)
    'e' -> rot1 $~! subtract 1
    'r' -> rot1 $~! (+1)
    't' -> rot2 $~! subtract 1
    'y' -> rot2 $~! (+1)
    'u' -> rot3 $~! subtract 1
    'i' -> rot3 $~! (+1)
    'm' -> zoom $~! (+1)
    'l' -> zoom $~! subtract 1
    'q' -> leaveMainLoop
    _   -> return ()

idle :: IdleCallback
idle = postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Conical Spiral"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient FrontAndBack $= black
  materialShininess FrontAndBack $= 95
  materialSpecular FrontAndBack $= white
  lighting $= Enabled
  lightModelTwoSide $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  depthFunc $= Just Less
  shadeModel $= Smooth
  n <- newIORef 5.0
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  displayCallback $= display rot1 rot2 rot3 n zoom
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 n zoom)
  idleCallback $= Just idle
  putStrLn "*** Haskell OpenGL Conical Spiral ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Increase/decrease parameter: n, b \n\
        \"
  mainLoop


