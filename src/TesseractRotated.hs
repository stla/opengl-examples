module TesseractRotated
  where
import           Data.IORef
import           Data.Tuple.Extra                  (both)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Tesseract.Data
import           Tesseract.Transformations4D
import           Utils.OpenGL                      (triangleNormal)
import           Utils.Prism
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Utils.ConvertPPM
import qualified Data.ByteString                   as B
import           Text.Printf
import           Control.Monad                     (when)

white,black,grey,whitesmoke :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1
grey       = Color4  0.8  0.8  0.8  0.7
whitesmoke = Color4 0.96 0.96 0.96    1

display :: IORef GLdouble -> IORef GLint -> DisplayCallback
display xxx save = do
  clear [ColorBuffer, DepthBuffer]
  x <- get xxx
  let points  = map (rotate4D 0.0 0.0 (x * pi / 180)) tesseractVertices
      ppoints = map project4D points
      vectors = map toVector3 ppoints
      edges   = map (both (toVertex3 . (!!) ppoints)) tesseractEdges
      ridges  = map (map (toVertex3 . (!!) ppoints)) tesseractFacet
      y = realToFrac x :: GLfloat
  s <- get save
  loadIdentity
  rotate y $ Vector3 1 (y/10) (y/10)
  mapM_ (\vec -> preservingMatrix $ do
                  translate vec
                  materialDiffuse Front $= whitesmoke
                  renderObject Solid $ Sphere' 0.2 50 50)
        vectors
  mapM_ (drawCylinder 0.1) edges
  mapM_ (renderPrimitive Quads . drawRidge) ridges
  when (s > 0 && s <= 183) $ do
    let ppm = printf "tesseract%04d.ppm" s
        png = printf "tesseract%04d.png" s
    (>>=) capturePPM (B.writeFile ppm)
    convert ppm png True
    save $~! (+1)
  swapBuffers
  where
    toVector3 x = Vector3 (x!!0) (x!!1) (x!!2)
    toVertex3 x = Vertex3 (x!!0) (x!!1) (x!!2)

drawRidge :: [Vertex3 GLdouble] -> IO ()
drawRidge vs = do
  materialDiffuse FrontAndBack $= grey
  normal (triangleNormal (vs!!0, vs!!1, vs!!2))
  mapM_ vertex vs

drawCylinder :: GLdouble -> (Vertex3 GLdouble, Vertex3 GLdouble) -> IO ()
drawCylinder radius (v1,v2) = do
  let cylinder = prism v1 v2 50 radius
  renderPrimitive Quads $ do
    materialDiffuse Back $= whitesmoke
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
  lookAt (Vertex3 (-6) 6 (-12)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLint -> KeyboardCallback
keyboard save c _ =
  case c of
    's' -> save $~! (+ 1)
    'q' -> leaveMainLoop
    _   -> return ()

idle :: IORef GLdouble -> IdleCallback
idle xxx = do
  xxx $~! (+ 2)
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
  xxx <- newIORef 0.0
  save <- newIORef 0
  displayCallback $= display xxx save
  reshapeCallback $= Just resize
  keyboardCallback $= Just (keyboard save)
  idleCallback $= Just (idle xxx)
  mainLoop
