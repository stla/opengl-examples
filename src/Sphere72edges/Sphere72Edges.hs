module Sphere72edges.Sphere72Edges
  where
import           Data.IORef
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Sphere72edges.Data
import           Utils.Prism
import           Utils.OpenGL                      (negateNormal, triangleNormal)


data Context = Context
    {
      contextRot1 :: IORef GLfloat
    , contextRot2 :: IORef GLfloat
    , contextRot3 :: IORef GLfloat
    }

white,black,red :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1
red        = Color4    1    0    0    1

display :: Context -> IORef GLdouble -> DisplayCallback
display context zoom = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  z <- get zoom
  loadIdentity
  (_, size) <- get viewport
  resize z size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  mapM_ drawLine edges
  renderPrimitive Polygon $ do
    materialDiffuse Front $= red
    mapM_ drawFace faces
  swapBuffers
  where
    drawFace face = do
      normal $ triangleNormal (face!!0, face!!1, face!!2)
      mapM_ vertex face
    drawLine (v1,v2) = do
      let cylinder = prism v1 v2 30 0.1
      renderPrimitive Quads $ do
        materialDiffuse Front $= red
        mapM_ drawQuad cylinder
      where
        drawQuad ((w1,w2,w3,w4),n) = do
          normal n
          vertex w1
          vertex w2
          vertex w3
          vertex w4

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-11+zoom)) (Vertex3 0 1 0) (Vector3 0 (-1) 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h


keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat
         -> IORef GLdouble -> KeyboardCallback
keyboard rot1 rot2 rot3 zoom c _ = do
  case c of
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


-- idle :: IORef Bool -> IORef Int -> IORef GLfloat -> IdleCallback
-- idle anim snapshots alpha = do
--     a <- get anim
--     s <- get snapshots
--     when a $ do
--       when (s < 360) $ do
--         let ppm = printf "ppm/heart%04d.ppm" s
--         (>>=) capturePPM (B.writeFile ppm)
--       snapshots $~! (+1)
--       alpha $~! (+1)
--       postRedisplay Nothing
--     return ()

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Heart"
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
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3}
                             zoom
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 zoom)
  idleCallback $= Nothing
  putStrLn "*** Da Vinci polyehdra ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \"
  mainLoop
