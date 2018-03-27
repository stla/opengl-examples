module Pentaract.Pentaract3 where
import           Control.Monad                     (when)
import qualified Data.ByteString                   as B
import           Data.IORef
-- import           Data.List
-- import           Data.List.Index                   (imap, imapM_)
import           Data.Tuple.Extra                  (both)
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Pentaract.Data
import           Tesseract.Transformations4D
import           Text.Printf
--import           Utils.Colour
--import           Utils.OpenGL                      (negateNormal,
--                                                    triangleNormal)
import           Utils.Prism

white,black,grey,whitesmoke,red :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1
grey       = Color4  0.8  0.8  0.8  0.7
whitesmoke = Color4 0.96 0.96 0.96    1
red        = Color4    1    0    0    1


display :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLdouble
        -> IORef GLdouble -> IORef GLdouble -> DisplayCallback
display rot1 rot2 rot3 angle angle2 zoom = do
  clear [ColorBuffer, DepthBuffer]
  a <- get angle2
  let points  = map (rotation4Dplane [sqrt 3 / sqrt 4,1/2,0,0,0] [0,0,0,0,1] (a * pi / 180)) cube5
      -- points' = map stereoprojectn' points
      ppoints = map project4D points
      -- vectors = map toVector3 ppoints
      edges   = map (both (toVertex3 . (!!) ppoints)) edgesIdxs
  alpha <- get angle
  r1 <- get rot1
  r2 <- get rot2
  r3 <- get rot3
  z <- get zoom
  loadIdentity
  (_, size) <- get viewport
  resize z size
  rotate alpha $ Vector3 1 1 (1::GLdouble)
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
--  mapM_ (\vec -> preservingMatrix $ do
--                 materialDiffuse Front $= grey
--                  translate (toVector3 vec)
--                  renderObject Solid $ Sphere' 0.05 30 30)
--       allVertices
  mapM_ (drawCylinder 0.075) edges
  swapBuffers
  where
    -- toVector3 x = Vector3 (x!!0) (x!!1) (x!!2)
    toVertex3 x = Vertex3 (x!!0) (x!!1) (x!!2)
    drawCylinder :: GLdouble -> (Vertex3 GLdouble, Vertex3 GLdouble) -> IO ()
    drawCylinder radius (v1,v2) = do
      let cylinder = prism v1 v2 15 radius
      renderPrimitive Quads $ do
        materialDiffuse FrontAndBack $= whitesmoke
        mapM_ f cylinder
      where
        f ((w1,w2,w3,w4),n) = do
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
  lookAt (Vertex3 0 0 (-12+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLdouble
         -> IORef GLdouble -> IORef Bool -> KeyboardCallback
keyboard rot1 rot2 rot3 angle2 zoom anim c _ =
  case c of
    'o' -> angle2 $~! subtract 1
    'p' -> angle2 $~! (+ 1)
    'r' -> rot1 $~! subtract 1
    't' -> rot1 $~! (+1)
    'f' -> rot2 $~! subtract 1
    'g' -> rot2 $~! (+1)
    'v' -> rot3 $~! subtract 1
    'b' -> rot3 $~! (+1)
    'm' -> zoom $~! (+1)
    'l' -> zoom $~! subtract 1
    'a' -> writeIORef anim True
    'q' -> leaveMainLoop
    _   -> return ()

idle :: IORef Bool -> IORef GLdouble -> IdleCallback
idle anim angle2 = do
  a <- get anim
  r <- get angle2
  when a $ do
    when (r < 360) $ do
      let ppm = printf "ppm/pentaract%04d.ppm" (round r :: Int)
      (>>=) capturePPM (B.writeFile ppm)
    angle2 $~! (+ 1)
  postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Pentaract"
  windowSize $= Size 400 400
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= black
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
  depthMask $= Enabled
  shadeModel $= Smooth
  blend $= Enabled    -- allow transparency
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  angle <- newIORef 0.0
  angle2 <- newIORef 0.0
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  anim <- newIORef False
  displayCallback $= display rot1 rot2 rot3 angle angle2 zoom
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 angle2 zoom anim)
  idleCallback $= Just (idle anim angle2)
  mainLoop

