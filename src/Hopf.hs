module Hopf
  where
import           Control.Monad                     (when)
import qualified Data.ByteString                   as B
import           Data.IORef
import           Data.List
import           Data.List.Index                   (imap, imapM_)
import           Data.Tuple.Extra                  (both)
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Hopf.Hopf
import           Tesseract.Transformations4D
import           Text.Printf
import           Utils.OpenGL                      (triangleNormal)
import           Utils.Prism
import Utils.Colour

white,black,grey,whitesmoke,red :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1
grey       = Color4  0.8  0.8  0.8  0.7
whitesmoke = Color4 0.96 0.96 0.96    1
red = Color4 1 0 0 1

tseq :: [Double]
tseq = [frac i n * 4 | i <- [0 .. n-1]]
  where
    n = 500
    frac i n = realToFrac i / realToFrac n

curve1 :: [[Double]]
curve1 = map (hopfinverse [1 / sqrt 3, 1 / sqrt 3, 1 / sqrt 3]) (map (+0.01) tseq)

proj3D1 :: [[Double]]
proj3D1 = map project4D curve1

curve2 :: [[Double]]
curve2 = map (hopfinverse [1 / sqrt 3, 1 / sqrt 3, 1 / sqrt 3]) tseq


-- xi <- seq(1, 2*pi, len=length(t))
-- colors <- randomcoloR::distinctColorPalette(length(t))
-- for(i in 1:length(xi)){
--   rcurve <- rotate4D(t(curve), pi/4,pi/4,xi[i])
--   proj3D2 <- apply(rcurve, 1, stereographic)
--   for(j in 1:(length(t)-1)){
--    segments3d(rbind(proj3D2[,j],proj3D2[,j+1]), col=colors[i], lwd=3)
--   }
--   rgl.snapshot(sprintf("hopf%04d.png", i))
-- }

display :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLdouble
        -> IORef Int -> IORef GLdouble -> DisplayCallback
display rot1 rot2 rot3 angle angle2 zoom = do
  clear [ColorBuffer, DepthBuffer]
  alpha <- get angle2
  r1 <- get rot1
  r2 <- get rot2
  r3 <- get rot3
  z <- get zoom
  lineWidth $= 3
  a <- get angle
  let points  = map (\alpha -> map (rotate4D (pi/4) (pi/4) alpha) curve2)
                [tseq !! i | i <- [0 .. alpha]]
  let ppoints = map (map project4D) points
  loadIdentity
  (_, size) <- get viewport
  resize z size
  rotate a $ Vector3 1 1 (1::GLdouble)
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  lighting $= Disabled
  (renderPrimitive Points . drawLine) proj3D1
  lighting $= Enabled
  -- let points  = map (rotate4D (pi/4) (pi/4) (alpha * pi / 180)) curve2
  --     ppoints = map project4D points
  lighting $= Disabled
  imapM_ (\i l -> renderPrimitive LineStrip $ drawLine' i l) ppoints
  lighting $= Enabled
  swapBuffers
  where
    drawLine :: [[Double]] -> IO ()
    drawLine vs = do
      color (Color3 1 1 0 :: Color3 GLfloat)
      -- normal $ triangleNormal (vsvx30, vsvx31, vsvx32)
      -- materialDiffuse FrontAndBack $= red
      mapM_ (vertex . toVertex3) vs
        where
        vsvx30 = toVertex3 (vs!!0)
        vsvx31 = toVertex3 (vs!!1)
        vsvx32 = toVertex3 (vs!!2)
        toVertex3 x = Vertex3 (x!!0) (x!!1) (x!!2)
    drawLine' :: Int -> [[Double]] -> IO ()
    drawLine' i vs = do
      color (if i `div` 10 == 9 then pickColor3 i else pickColor3 1)
      mapM_ (vertex . toVertex3) vs
        where
        toVertex3 x = Vertex3 (x!!0) (x!!1) (x!!2)

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-21+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef Int
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

idle :: IORef Bool -> IORef Int -> IdleCallback
idle anim angle2 = do
  a <- get anim
  r <- get angle2
  when a $ do
    when (r < 360) $ do
      let ppm = printf "hopffibration%04d.ppm" r
      (>>=) capturePPM (B.writeFile ppm)
    angle2 $~! (+ 1)
  postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Hopf fibration"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient FrontAndBack $= Color4 0 0 0 0
  materialShininess FrontAndBack $= 50
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
  lightModelTwoSide $= Enabled
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= black
  depthFunc $= Just Lequal
  depthMask $= Enabled
  shadeModel $= Smooth
  blend $= Enabled    -- allow transparency
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  angle <- newIORef 0.0
  angle2 <- newIORef 0
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
