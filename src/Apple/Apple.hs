module Apple.Apple where
import           Control.Monad                     (when)
import qualified Data.ByteString                   as B
import           Data.IORef
import qualified Data.Map.Strict                   as M
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Apple.Data
import           Text.Printf
import           Utils.Palettes                    (colorRampSymmetric')

data Context = Context
    {
      contextRot1    :: IORef GLfloat
    , contextRot2    :: IORef GLfloat
    , contextRot3    :: IORef GLfloat
    , contextZoom    :: IORef Double
    , contextPalette :: IORef [Color4 GLfloat]
    , contextQuads   :: IORef [((Int,Int), Quad)]
    }

white,black :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1

n_u,n_v :: Int
n_u = 200
n_v = 200

surface :: Double -> Double -> [((Int,Int), Quad)]
surface r1 r2 = M.toList $ allQuads n_u n_v r1 r2

display :: Context -> DisplayCallback
display context = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  z <- get (contextZoom context)
  (_, size) <- get viewport
  colors <- get (contextPalette context)
  surf <- get (contextQuads context)
--  let colors = colorRampSymmetric' palette' n_u 0
--      surf = surface a' n' m'
  loadIdentity
  resize z size
  rotate (r1-120) $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  renderPrimitive Quads $ mapM_ (drawQuad colors) surf
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
  lookAt (Vertex3 0 0 (-25+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

--updateQuads :: Context -> IORef GLdouble -> IO ()
--updateQuads context (

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef GLdouble -> IORef GLdouble -- parameters r1, r2
         -> IORef GLdouble -- zoom
         -> IORef Bool     -- animation
         -> IORef [((Int,Int), Quad)]
         -> KeyboardCallback
keyboard rot1 rot2 rot3 r1 r2 zoom anim quadsRef c _ = do
  case c of
    'a' -> writeIORef anim True
    'v' -> do
      r1 $~! subtract 0.1
      r1' <- get r1
      r2' <- get r2
      let quads = surface r1' r2'
      writeIORef quadsRef quads
    'f' -> do
      r1 $~! (+ 0.1)
      r1' <- get r1
      r2' <- get r2
      let quads = surface r1' r2'
      writeIORef quadsRef quads
    'b' -> do
      r2 $~! subtract 0.1
      r1' <- get r1
      r2' <- get r2
      let quads = surface r1' r2'
      writeIORef quadsRef quads
    'g' -> do
      r2 $~! (+ 0.1)
      r1' <- get r1
      r2' <- get r2
      let quads = surface r1' r2'
      writeIORef quadsRef quads
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
  postRedisplay Nothing
-- idle :: IORef Bool -> IORef GLdouble -> IORef GLdouble -> IORef GLdouble
--      -> IORef [((Int,Int), Quad)] -> IORef Int -> IdleCallback
-- idle anim a n m quadsRef snapshots = do
--     anim' <- get anim
--     s <- get snapshots
--     when anim' $ do
--       when (s < 40) $ do
--         let ppm = printf "ppm/kleinbottlegray%04d.ppm" s
--         (>>=) capturePPM (B.writeFile ppm)
--       if s < 10
--         then
--           n $~! (+ 1)
--         else if s < 30
--           then
--             n $~! subtract 1
--           else if s < 40
--             then
--               n $~! (+ 1)
--             else return ()
--       snapshots $~! (+ 1)
--       a' <- get a
--       n' <- get n
--       m' <- get m
--       let quads = surface a' n' m'
--       writeIORef quadsRef quads
--     postRedisplay Nothing

menuPalette :: String -> IORef [Color4 GLfloat] -> MenuCallback
menuPalette palette colorsRef = do
  writeIORef colorsRef (colorRampSymmetric' palette n_u 0)
  postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Apple"
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
  let colors = colorRampSymmetric' "viridis" n_u 0
  colorsRef <- newIORef colors
  r1 <- newIORef 3.5
  r2 <- newIORef 3.5
  let quads = surface 3.5 3.5
  quadsRef <- newIORef quads
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
--  palette <- newIORef "viridis"
  anim <- newIORef False
  snapshots <- newIORef 0
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextZoom = zoom,
                                      contextPalette = colorsRef,
                                      contextQuads = quadsRef}
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 r1 r2 zoom anim quadsRef)
  idleCallback $= Nothing -- Just (idle anim a n m quadsRef snapshots)
  putStrLn "*** Apple ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Increase/decrease parameters:\n\
        \        f, v, g, b\n\
        \    Animation: a \n\
        \"
  attachMenu LeftButton
    (Menu [ SubMenu "Color palette"
                    (Menu [ MenuEntry "magma"    (menuPalette "magma" colorsRef)
                          , MenuEntry "inferno"  (menuPalette "inferno" colorsRef)
                          , MenuEntry "plasma"   (menuPalette "plasma" colorsRef)
                          , MenuEntry "viridis"  (menuPalette "viridis" colorsRef)
                          , MenuEntry "cviridis" (menuPalette "cviridis" colorsRef)
                          ])
          ]
      )
  mainLoop
