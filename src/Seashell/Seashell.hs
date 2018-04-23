module Seashell.Seashell where
import           Control.Monad                     (when)
import qualified Data.ByteString                   as B
import           Data.IORef
import qualified Data.Map.Strict                   as M
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Seashell.Data
import           Text.Printf
import           Utils.Palettes                    (colorRamp')

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
n_u = 300
n_v = 300

surface :: Double -> Double -> [((Int,Int), Quad)]
surface h n = M.toList $ allQuads n_u n_v h n

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
  loadIdentity
  resize z size
  rotate (r1+70) $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate (r3-135) $ Vector3 0 0 1
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
  lookAt (Vertex3 0 0 (-10+zoom)) (Vertex3 0 (-2) 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef GLdouble -> IORef GLdouble -- parameters h and n
         -> IORef GLdouble -- zoom
         -> IORef Bool     -- animation
         -> IORef [((Int,Int), Quad)]
         -> KeyboardCallback
keyboard rot1 rot2 rot3 h n zoom anim quadsRef c _ = do
  case c of
    'a' -> writeIORef anim True
    'v' -> do
      h $~! subtract 0.1
      h' <- get h
      n' <- get n
      let quads = surface h' n'
      writeIORef quadsRef quads
    'f' -> do
      h $~! (+ 0.1)
      h' <- get h
      n' <- get n
      let quads = surface h' n'
      writeIORef quadsRef quads
    'b' -> do
      n $~! (\x -> if x>1.0 then x-1.0 else x)
      h' <- get h
      n' <- get n
      let quads = surface h' n'
      writeIORef quadsRef quads
    'g' -> do
      n $~! (+ 1.0)
      h' <- get h
      n' <- get n
      let quads = surface h' n'
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
menuPalette palette colorsRef =
  writeIORef colorsRef (colorRamp' palette n_u)

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Seashell"
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
  let colors = colorRamp' "viridis" n_u
  colorsRef <- newIORef colors
  let quads = surface 2.0 2.0
  quadsRef <- newIORef quads
  h <- newIORef 2.0
  n <- newIORef 2.0
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  anim <- newIORef False
  snapshots <- newIORef 0
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextZoom = zoom,
                                      contextPalette = colorsRef,
                                      contextQuads = quadsRef}
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 h n zoom anim quadsRef)
  idleCallback $= Nothing -- Just (idle anim a n m quadsRef snapshots)
  putStrLn "*** Seashell ***\n\
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
