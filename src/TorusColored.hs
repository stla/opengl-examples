module Torus
  where
import           Data.IORef
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Utils.Colour                 (interpolateColor)
import           Utils.Torus

white,black,blue,red,green :: Color4 GLfloat
white = Color4 1   1   1   1
black = Color4 0   0   0   1
blue  = Color4 0   0   1   1
red   = Color4 1   0   0   1
green = Color4 0   1   0   1

vertexColor :: Vertex3 Double -> Double -> Color4 GLfloat
vertexColor v radius = interpolateColor (0,0,1) (1,1,0) (1,0,0) (ycoord v)
  where
    ycoord (Vertex3 _ y _) = realToFrac ((y+radius)/(2*radius))

display :: IORef GLfloat -> IORef GLfloat -> IORef GLdouble -> IORef GLdouble
        -> DisplayCallback
display rot1 rot2 zoom tradius = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get rot1
  r2 <- get rot2
  z <- get zoom
  tr <- get tradius
  let tr' = max 0 (min tr 2)
  let torus1 = torus (2-tr') tr' 180 90
  loadIdentity
  rotate (-40::GLfloat) $ Vector3 1 0 0
  (_, size) <- get viewport
  resize z size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  drawTorus torus1 tr'
  swapBuffers
  where
    drawTorus quads radius =
      (renderPrimitive Quads . mapM_ drawTorusFacet) quads
      where
        drawTorusFacet ((v1,v2,v3,v4),n) = do
          normal n
          materialDiffuse Front $= vertexColor v1 radius
          vertex v1
          materialDiffuse Front $= vertexColor v2 radius
          vertex v2
          materialDiffuse Front $= vertexColor v3 radius
          vertex v3
          materialDiffuse Front $= vertexColor v4 radius
          vertex v4

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-13 + zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboardAndMouse :: IORef GLfloat -> IORef GLfloat -> IORef GLdouble
                 -> IORef GLdouble -> KeyboardMouseCallback
keyboardAndMouse rot1 rot2 zoom tradius key keyState _ _ =
  case (key, keyState) of
    (Char 'j', _)                   -> rot1 $~! subtract 1
    (Char 'k', _)                   -> rot1 $~! (+1)
    (Char 'h', _)                   -> rot2 $~! subtract 1
    (Char 'l', _)                   -> rot2 $~! (+1)
    (Char 'o', _)                   -> tradius $~! subtract 0.025
    (Char 'p', _)                   -> tradius $~! (+0.025)
    (Char 'q', _)                   -> leaveMainLoop
    (MouseButton LeftButton, Down)  -> zoom $~! (+0.1)
    (MouseButton RightButton, Down) -> zoom $~! subtract 0.1
    _                               -> return ()

idle :: IdleCallback
idle = postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Torus"
  windowSize $= Size 600 600
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient Front $= Color4 0.5 0.5 0.5 1
  materialShininess Front $= 30
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 100 0 (-100) 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= black
  depthFunc $= Just Lequal
  depthMask $= Enabled
  shadeModel $= Smooth
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  zoom <- newIORef 0.0
  tradius <- newIORef 1.0
  displayCallback $= display rot1 rot2 zoom tradius
  reshapeCallback $= Just (resize 0)
  keyboardMouseCallback $= Just (keyboardAndMouse rot1 rot2 zoom tradius)
  idleCallback $= Just idle
  mainLoop
