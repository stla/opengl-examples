module CGAL02
  where
-- import           CGAL.CGAL02
import qualified Data.ByteString                   as B
import           Data.IORef
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Text.Printf
import           Utils.ConvertPPM
import           Utils.OpenGL                      (triangleNormal)

white,black,sienna,red,green :: Color4 GLfloat
white  = Color4 1    1    1    1
black  = Color4 0    0    0    1
sienna = Color4 0.63 0.32 0.18 0.4
red    = Color4 1    0    0    0.5
green  = Color4 0    1    0    0.5

vertices :: [Vertex3 GLfloat]
vertices = [Vertex3 (-1.0) (-1.0) (-1.0),Vertex3 (-1.0) (-1.0) 1.0,Vertex3 1.0 (-1.0) 1.0,Vertex3 1.0 (-1.0) (-1.0),Vertex3 1.0 1.0 (-1.0),Vertex3 (-1.0) 1.0 (-1.0),Vertex3 (-1.0) 1.0 1.0,Vertex3 1.0 1.0 1.0,Vertex3 (-0.5) (-0.5) (-0.5),Vertex3 (-0.5) (-0.5) 0.5,Vertex3 0.5 (-0.5) 0.5,Vertex3 0.5 (-0.5) (-0.5),Vertex3 0.5 0.5 (-0.5),Vertex3 (-0.5) 0.5 (-0.5),Vertex3 (-0.5) 0.5 0.5,Vertex3 0.5 0.5 0.5]
polygons :: [[Vertex3 GLfloat]]
polygons = [[Vertex3 (-1.0) (-1.0) 1.0,Vertex3 (-1.0) (-1.0) (-1.0),Vertex3 1.0 (-1.0) (-1.0),Vertex3 1.0 (-1.0) 1.0],[Vertex3 1.0 (-1.0) (-1.0),Vertex3 (-1.0) (-1.0) (-1.0),Vertex3 (-1.0) 1.0 (-1.0),Vertex3 1.0 1.0 (-1.0)],[Vertex3 (-1.0) 1.0 (-1.0),Vertex3 (-1.0) (-1.0) (-1.0),Vertex3 (-1.0) (-1.0) 1.0,Vertex3 (-1.0) 1.0 1.0],[Vertex3 (-1.0) 1.0 1.0,Vertex3 (-1.0) (-1.0) 1.0,Vertex3 1.0 (-1.0) 1.0,Vertex3 1.0 1.0 1.0],[Vertex3 1.0 (-1.0) 1.0,Vertex3 1.0 (-1.0) (-1.0),Vertex3 1.0 1.0 (-1.0),Vertex3 1.0 1.0 1.0],[Vertex3 1.0 1.0 (-1.0),Vertex3 (-1.0) 1.0 (-1.0),Vertex3 (-1.0) 1.0 1.0,Vertex3 1.0 1.0 1.0],[Vertex3 0.5 (-0.5) 0.5,Vertex3 0.5 (-0.5) (-0.5),Vertex3 (-0.5) (-0.5) (-0.5),Vertex3 (-0.5) (-0.5) 0.5],[Vertex3 0.5 0.5 (-0.5),Vertex3 (-0.5) 0.5 (-0.5),Vertex3 (-0.5) (-0.5) (-0.5),Vertex3 0.5 (-0.5) (-0.5)],[Vertex3 (-0.5) 0.5 0.5,Vertex3 (-0.5) (-0.5) 0.5,Vertex3 (-0.5) (-0.5) (-0.5),Vertex3 (-0.5) 0.5 (-0.5)],[Vertex3 0.5 0.5 0.5,Vertex3 0.5 (-0.5) 0.5,Vertex3 (-0.5) (-0.5) 0.5,Vertex3 (-0.5) 0.5 0.5],[Vertex3 0.5 0.5 0.5,Vertex3 0.5 0.5 (-0.5),Vertex3 0.5 (-0.5) (-0.5),Vertex3 0.5 (-0.5) 0.5],[Vertex3 0.5 0.5 0.5,Vertex3 (-0.5) 0.5 0.5,Vertex3 (-0.5) 0.5 (-0.5),Vertex3 0.5 0.5 (-0.5)]]

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
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  mapM_ (\v -> preservingMatrix $ do
                  translate $ toVec v
                  materialDiffuse Front $= sienna
                  renderObject Solid $ Sphere' 0.1 50 50)
        vertices
  mapM_ (renderPrimitive Polygon . drawPolygon) (take 6 polygons)
  mapM_ (renderPrimitive Polygon . drawPolygon') (drop 6 polygons)
  swapBuffers
  where
    toVec (Vertex3 x y z) = Vector3 x y z

drawPolygon :: [Vertex3 GLfloat] -> IO ()
drawPolygon xs = do
  materialDiffuse FrontAndBack $= red
  normal (triangleNormal (xs!!0, xs!!1, xs!!2))
  mapM_ vertex xs

drawPolygon' :: [Vertex3 GLfloat] -> IO ()
drawPolygon' xs = do
  materialDiffuse FrontAndBack $= green
  normal (triangleNormal (xs!!0, xs!!1, xs!!2))
  mapM_ vertex xs

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1 100.0
  lookAt (Vertex3 0 0 (-9 + zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboardAndMouse :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat
                 -> IORef GLint -> IORef GLdouble -> KeyboardMouseCallback
keyboardAndMouse rot1 rot2 rot3 capture zoom key keyState _ _ =
  case (key, keyState) of
    (Char 'r', _) -> rot1 $~! subtract 1
    (Char 't', _) -> rot1 $~! (+1)
    (Char 'f', _) -> rot2 $~! subtract 1
    (Char 'g', _) -> rot2 $~! (+1)
    (Char 'v', _) -> rot3 $~! subtract 1
    (Char 'b', _) -> rot3 $~! (+1)
    (Char 'c', _) -> do
      i <- get capture
      let ppm = printf "cgal02%04d.ppm" i
          png = printf "cgal02%04d.png" i
      (>>=) capturePPM (B.writeFile ppm)
      convert ppm png True
      capture $~! (+1)
    (Char 'q', _) -> leaveMainLoop
    (MouseButton LeftButton, Down)  -> zoom $~! (+0.1)
    (MouseButton RightButton, Down) -> zoom $~! subtract 0.1
    _   -> return ()

idle :: IdleCallback
idle = postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "CGAL02"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient Front $= black
  materialShininess Front $= 10
  lighting $= Enabled
  lightModelTwoSide $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= black
  depthFunc $= Just Less
  depthMask $= Enabled
  shadeModel $= Smooth
  blend $= Enabled    -- allow transparency
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  capture <- newIORef 1
  displayCallback $= display rot1 rot2 rot3 zoom
  reshapeCallback $= Just (resize 0)
  keyboardMouseCallback $= Just (keyboardAndMouse rot1 rot2 rot3 capture zoom)
  idleCallback $= Just idle
  mainLoop
