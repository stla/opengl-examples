module ConicalSpiral.ConicalSpiral5 where
import           ConicalSpiral.Data4
import           Data.IORef
import qualified Data.Map.Strict              as M
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Utils.Palettes               (colorRamp')

white,black :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1

n_u,n_v :: Int
n_u = 200
n_v = 200


display :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
        -> IORef String    -- color palette
        -> IORef GLdouble  -- parameter n
        -> IORef GLdouble  -- zoom
        -> DisplayCallback
display rot1 rot2 rot3 palette n zoom = do
  clear [ColorBuffer, DepthBuffer]
  n' <- get n
  r1 <- get rot1
  r2 <- get rot2
  r3 <- get rot3
  z <- get zoom
  (_, size) <- get viewport
  palette' <- get palette
  let colors = colorRamp' palette' n_v
      surface = allQuads n_u n_v n'
  loadIdentity
  resize z size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  renderPrimitive Quads $ mapM_ (drawQuad colors) (M.toList surface)
  swapBuffers
  where
    drawQuad thecolors ((_,j), quad) = do
      materialDiffuse FrontAndBack $= thecolors !! j
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
    'e' -> rot1 $~! subtract 2
    'r' -> rot1 $~! (+2)
    't' -> rot2 $~! subtract 2
    'y' -> rot2 $~! (+2)
    'u' -> rot3 $~! subtract 2
    'i' -> rot3 $~! (+2)
    'm' -> zoom $~! (+1)
    'l' -> zoom $~! subtract 1
    'q' -> leaveMainLoop
    _   -> return ()

idle :: IdleCallback
idle = postRedisplay Nothing

menuPalette :: IORef String -> String -> MenuCallback
menuPalette = writeIORef

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
  palette <- newIORef "viridis"
  displayCallback $= display rot1 rot2 rot3 palette n zoom
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
  attachMenu LeftButton
    (Menu [ SubMenu "Color palette"
                    (Menu [ MenuEntry "magma"    (menuPalette palette "magma")
                          , MenuEntry "inferno"  (menuPalette palette "inferno")
                          , MenuEntry "plasma"   (menuPalette palette "plasma")
                          , MenuEntry "viridis"  (menuPalette palette "viridis")
                          , MenuEntry "cviridis" (menuPalette palette "cviridis")
                          ])
          ]
      )
  mainLoop


