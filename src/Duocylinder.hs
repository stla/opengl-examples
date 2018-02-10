module Duocylinder
  where
import           Control.Monad                     (when)
import qualified Data.ByteString                   as B
import           Data.IORef
import           Data.List.Index                   (imapM_)
import           Data.Tuple.Extra                  (both)
import           Duocylinder.Data
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Tesseract.Transformations4D
import           Text.Printf
import           Utils.Prism
import           Utils.Colour                      (pickColor)
import           Utils.OpenGL                      (triangleNormal)

white,black,red,grey,whitesmoke :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1
red        = Color4    1    0    0    1
grey       = Color4  0.8  0.8  0.8  0.7
whitesmoke = Color4 0.96 0.96 0.96    1

display :: IORef GLdouble -> DisplayCallback
display angle = do
  clear [ColorBuffer, DepthBuffer]
  alpha <- get angle
  let points  = map (rotate4D 0 0 (alpha * pi / 180)) dcVertices
      ppoints = map project4D points
      vectors = map toVector3 ppoints
      edges   = map (both (toVertex3 . (!!) ppoints)) dcEdges
      facets  = take 1 $ map (map (map (toVertex3 . (!!) ppoints))) dcFacets
  loadIdentity
  mapM_ (\vec -> preservingMatrix $ do
                  translate vec
                  materialDiffuse Front $= whitesmoke
                  renderObject Solid $ Sphere' 0.1 15 15)
        vectors
  mapM_ (drawCylinder whitesmoke 0.05) edges
  imapM_ (\i ridges -> mapM_ (renderPrimitive Polygon . drawRidge i) ridges) facets
  swapBuffers
  where
    toVector3 x = Vector3 (x!!0) (x!!1) (x!!2)
    toVertex3 x = Vertex3 (x!!0) (x!!1) (x!!2)

drawRidge :: Int -> [Vertex3 GLdouble] -> IO ()
drawRidge i vs = do
  materialDiffuse FrontAndBack $= grey
  normal (triangleNormal (vs!!0, vs!!1, vs!!2))
  mapM_ vertex vs

drawCylinder :: Color4 GLfloat -> GLdouble
             -> (Vertex3 GLdouble, Vertex3 GLdouble) -> IO ()
drawCylinder col radius (v1,v2) = do
  let cylinder = prism v1 v2 15 radius
  renderPrimitive Quads $ do
    materialDiffuse Back $= col
    mapM_ f cylinder
  where
    f ((w1,w2,w3,w4),n) = do
      normal n
      vertex w1
      vertex w2
      vertex w3
      vertex w4

resize :: Size -> IO ()
resize s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 (-31) 31 (-62)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLdouble -> IORef Bool -> KeyboardCallback
keyboard angle anim c _ =
  case c of
    'a' -> writeIORef anim True
    's' -> writeIORef anim False
    'o' -> angle $~! subtract 1
    'p' -> angle $~! (+ 1)
    'q' -> leaveMainLoop
    _   -> return ()

idle :: IORef Bool -> IORef GLdouble -> IdleCallback
idle anim angle = do
  a <- get anim
  r <- get angle
  when a $ do
    when (r < 360) $ do
      let ppm = printf "pic%04d.ppm" (round r :: Int)
      (>>=) capturePPM (B.writeFile ppm)
    angle $~! (+ 1)
  postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Tesseract"
  windowSize $= Size 600 600
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= Color4 0 0 0 0
  materialAmbient FrontAndBack $= Color4 0 0 0 0
  materialShininess FrontAndBack $= 50
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 (-6) 6 (-12) 1
  lightModelTwoSide $= Enabled
  ambient (Light 0) $= white
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  depthFunc $= Just Lequal
  depthMask $= Enabled
  shadeModel $= Smooth
  blend $= Enabled    -- allow transparency
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  angle <- newIORef 0.0
  anim <- newIORef False
  displayCallback $= display angle
  reshapeCallback $= Just resize
  keyboardCallback $= Just (keyboard angle anim)
  idleCallback $= Just (idle anim angle)
  mainLoop
