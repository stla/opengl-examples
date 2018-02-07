module HorseSaddle3
  where
import           Data.IORef
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Utils.Colour                 (interpolateColor)
import           Utils.HorseSaddle
import           Utils.PrismaticPath          (prismaticPath')

type Quad = (Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double)

white,black,green,red :: Color4 GLfloat
white = Color4 1   1   1   1
black = Color4 0   0   0   1
green = Color4 0   1   0   1
red   = Color4 1   0   0   1

saddle :: ([(Quad, Normal3 Double)], [[Vertex3 Double]])
saddle = horseSaddle 30 30
quads :: [(Quad, Normal3 Double)]
quads = fst saddle
paths :: [(Quad, Normal3 Double)]
paths = concatMap (\vs -> prismaticPath' vs 3 0.005 False) (snd saddle)

extractz :: Vertex3 Double -> Double
extractz (Vertex3 _ _ z) = z

zvalues :: [Double]
zvalues = let zvalue (v1, v2, v3, v4) = map extractz [v1,v2,v3,v4] in
  concatMap (zvalue . fst) quads
zmin :: Double
zmin = minimum zvalues
zmax :: Double
zmax = maximum zvalues
zrange :: Double
zrange = zmax - zmin

zrelative :: Vertex3 Double -> GLfloat
zrelative v = realToFrac $ (extractz v - zmin) / zrange

zcolor :: Vertex3 Double -> Color4 GLfloat
zcolor v = interpolateColor (0,0,1) (1,1,0) (1,0,0) (zrelative v)

display :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLdouble
        -> DisplayCallback
display rot1 rot2 rot3 zoom = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get rot1
  r2 <- get rot2
  r3 <- get rot3
  z <- get zoom
  loadIdentity
  (_, size) <- get viewport
  resize z size
  rotate (-30::GLfloat) $ Vector3 0 0 1
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  renderPrimitive Quads $ do
    materialDiffuse FrontAndBack $= black
    mapM_ drawQuad paths
  renderPrimitive Quads $
    mapM_ drawQuad quads
  swapBuffers
  where
    drawQuad ((v1,v2,v3,v4),n) = do
      normal n
      materialDiffuse Back $= zcolor v1
      vertex v1
      materialDiffuse Back $= zcolor v2
      vertex v2
      materialDiffuse Back $= zcolor v3
      vertex v3
      materialDiffuse Back $= zcolor v4
      vertex v4

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 (4 - zoom) 0) (Vertex3 0 0 0) (Vector3 0 0 1)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboardAndMouse :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat
                 -> IORef GLdouble -> KeyboardMouseCallback
keyboardAndMouse rot1 rot2 rot3 zoom key keyState _ _ =
  case (key, keyState) of
    (Char 'e', _)                   -> rot1 $~! subtract 1
    (Char 'r', _)                   -> rot1 $~! (+1)
    (Char 'd', _)                   -> rot2 $~! subtract 1
    (Char 'f', _)                   -> rot2 $~! (+1)
    (Char 'c', _)                   -> rot3 $~! subtract 1
    (Char 'v', _)                   -> rot3 $~! (+1)
    (Char 'q', _)                   -> leaveMainLoop
    (MouseButton LeftButton, Down)  -> zoom $~! (+0.1)
    (MouseButton RightButton, Down) -> zoom $~! subtract 0.1
    _                               -> return ()

idle :: IdleCallback
idle = postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Horse saddle"
  windowSize $= Size 600 600
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient Front $= Color4 0.5 0.5 0.5 1
  materialShininess Front $= 3
  lighting $= Enabled
  lightModelTwoSide $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 100 0 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= black
  depthFunc $= Just Lequal
  depthMask $= Enabled
  shadeModel $= Flat
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  displayCallback $= display rot1 rot2 rot3 zoom
  reshapeCallback $= Just (resize 0)
  keyboardMouseCallback $= Just (keyboardAndMouse rot1 rot2 rot3 zoom)
  idleCallback $= Just idle
  mainLoop
