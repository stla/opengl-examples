module EllipticTorus.EllipticTorus where
import           Control.Monad                     (when)
import qualified Data.ByteString                   as B
import           Data.IORef
import qualified Data.Map.Strict                   as M
import           EllipticTorus.Data
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT                  hiding (shift)
import           Text.Printf
import           Utils.Palettes                    (colorRampSymmetric')

white,black :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1

n_u,n_v :: Int
n_u = 200
n_v = 200


display :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
        -> IORef String    -- color palette
        -> IORef Int       -- shift palette
        -> IORef GLdouble -> IORef GLdouble -- parameter a and b
        -> IORef GLdouble  -- zoom
        -> DisplayCallback
display rot1 rot2 rot3 palette shift a b zoom = do
  clear [ColorBuffer, DepthBuffer]
  a' <- get a
  b' <- get b
  r1 <- get rot1
  r2 <- get rot2
  r3 <- get rot3
  z <- get zoom
  (_, size) <- get viewport
  palette' <- get palette
  shift' <- get shift
  let colors = colorRampSymmetric' palette' n_u (shift' `mod` n_u)
      surface = M.toList $ allQuads n_u n_v a' b' 4
  loadIdentity
  resize z size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  renderPrimitive Quads $ mapM_ (drawQuad colors) surface
  swapBuffers
  where
    drawQuad thecolors ((i,_), quad) = do
      materialDiffuse FrontAndBack $= thecolors !! i
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
  lookAt (Vertex3 0 0 (-18+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef Int      -- shift palette
         -> IORef GLdouble -> IORef GLdouble -- parameters a and b
         -> IORef GLdouble -- zoom
         -> IORef Bool     -- animation
         -> KeyboardCallback
keyboard rot1 rot2 rot3 shift a b zoom anim c _ =
  case c of
    'a' -> writeIORef anim True
    's' -> shift $~! (+2)
    'c' -> a $~! subtract 0.1
    'v' -> a $~! (+ 0.1)
    'b' -> b $~! subtract 0.1
    'n' -> b $~! (+ 0.1)
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

idle :: IORef Bool -> IORef GLdouble -> IORef GLdouble -> IORef Int -> IdleCallback
idle anim a b snapshots = do
    anim' <- get anim
    s <- get snapshots
    when anim' $ do
      when (s < 40) $ do
        let ppm = printf "ppm/elliptictorus%04d.ppm" s
        (>>=) capturePPM (B.writeFile ppm)
      if s < 10
        then do
          a $~! (+ 0.1)
          b $~! subtract 0.1
        else if s < 30
          then do
            a $~! subtract 0.1
            b $~! (+ 0.1)
          else if s < 40
            then do
              a $~! (+ 0.1)
              b $~! subtract 0.1
            else return ()
      snapshots $~! (+ 1)
    postRedisplay Nothing

menuPalette :: IORef String -> String -> MenuCallback
menuPalette = writeIORef

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Elliptic torus"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient FrontAndBack $= black
--  materialShininess FrontAndBack $= 95
--  materialSpecular FrontAndBack $= white
  lighting $= Enabled
  lightModelTwoSide $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= black
  depthFunc $= Just Less
  shadeModel $= Smooth
  a <- newIORef 1.0
  b <- newIORef 1.0
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  angle <- newIORef 0.0
  palette <- newIORef "viridis"
  shift <- newIORef 0
  anim <- newIORef False
  snapshots <- newIORef 0
  displayCallback $= display rot1 rot2 rot3 palette shift a b zoom
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 shift a b zoom anim)
  idleCallback $= Just (idle anim a b snapshots)
  putStrLn "*** Haskell OpenGL Elliptic Torus ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Increase/decrease parameters: c, v, n, b \n\
        \    Rotate colors: s \n\
        \    Animation: a \n\
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


