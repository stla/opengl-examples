module KleinBottle4D.KleinBottle4D where
import           Control.Monad                     (when)
import qualified Data.ByteString                   as B
import           Data.IORef
import           Data.List
import           Data.List.Index                   (imap, imapM_)
import           Data.Tuple.Extra                  (both)
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           KleinBottle4D.Data
import           Text.Printf
-- import           Utils.Colour
import           Utils.OpenGL                      (negateNormal,
                                                    triangleNormal)
import           Utils.Quads.Color

white,black,grey,whitesmoke,red :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1
grey       = Color4  0.8  0.8  0.8  0.7
whitesmoke = Color4 0.96 0.96 0.96    1
red        = Color4    1    0    0    1

display :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLdouble
        -> IORef GLdouble -> IORef GLdouble -> IORef String -> DisplayCallback
display rot1 rot2 rot3 angle angle2 zoom plane = do
  clear [ColorBuffer, DepthBuffer]
  alpha <- get angle2
  r1 <- get rot1
  r2 <- get rot2
  r3 <- get rot3
  z <- get zoom
  a <- get angle
  p <- get plane
  let klein = allQuads 4 4 (alpha*pi/180) p
  loadIdentity
  (_, size) <- get viewport
  resize z size
--  rotate alpha $ Vector3 1 1 (1::GLdouble)
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  renderPrimitive Quads $
--    materialDiffuse FrontAndBack $= red
    mapM_ drawQuad klein
  swapBuffers
  where
    drawQuad ((v1,v2,v3,v4),n) = do
      materialDiffuse FrontAndBack $= quadColor (v1,v2,v3,v4) Nothing
      normal n
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
  lookAt (Vertex3 0 0 (-40+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLdouble
         -> IORef GLdouble -> IORef Bool -> IORef String -> KeyboardCallback
keyboard rot1 rot2 rot3 angle2 zoom anim plane c _ =
  case c of
    'o' -> angle2 $~! subtract 1
    'p' -> angle2 $~! (+ 1)
    'e' -> rot1 $~! subtract 1
    'r' -> rot1 $~! (+1)
    't' -> rot2 $~! subtract 1
    'y' -> rot2 $~! (+1)
    'u' -> rot3 $~! subtract 1
    'i' -> rot3 $~! (+1)
    'm' -> zoom $~! (+1)
    'l' -> zoom $~! subtract 1
    'a' -> writeIORef anim True
    'd' -> writeIORef plane "XY"
    'f' -> writeIORef plane "XZ"
    'g' -> writeIORef plane "XU"
    'h' -> writeIORef plane "YZ"
    'j' -> writeIORef plane "YU"
    'k' -> writeIORef plane "ZU"
    'q' -> leaveMainLoop
    _   -> return ()

menuZoomPlus :: IORef GLdouble -> MenuCallback
menuZoomPlus zoom = zoom $~! (+1)

menuZoomMinus :: IORef GLdouble -> MenuCallback
menuZoomMinus zoom = zoom $~! subtract 1

menuRotationPlane :: IORef String -> String -> MenuCallback
menuRotationPlane ioplane plane = writeIORef ioplane plane

idle :: IORef Bool -> IORef GLdouble -> IdleCallback
idle anim angle2 = do
  a <- get anim
  r <- get angle2
  when a $ do
    when (r < 360) $ do
      let ppm = printf "ppm/kleinbottle4D%04d.ppm" (round r :: Int)
      (>>=) capturePPM (B.writeFile ppm)
    angle2 $~! (+ 1)
  postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "4D Klein bottle"
  windowSize $= Size 400 400
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient FrontAndBack $= black
  materialShininess FrontAndBack $= 95
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
  plane <- newIORef "XY"
  displayCallback $= display rot1 rot2 rot3 angle angle2 zoom plane
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 angle2 zoom anim plane)
  idleCallback $= Just (idle anim angle2)
  tabletCallback $= Just tablet
  dialAndButtonBoxCallback $= Just (dial zoom)
  putStrLn "*** Haskell OpenGL 4D Klein Bottle ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Rotate in 4 dimensions: o, p\n\
        \    Plane of rotation:\n\
        \        d, f, g, h, j, k\n\
        \    Zoom: l, m\n\
        \    Animation: a\n\
        \"
  attachMenu LeftButton
             (Menu [ SubMenu "Zoom" (Menu [ MenuEntry "Zoom+" (menuZoomPlus zoom)
                                          , MenuEntry "Zoom-" (menuZoomMinus zoom)
                                          ])
                   , SubMenu "Rotation plane"
                             (Menu [ MenuEntry "XY" (menuRotationPlane plane "XY")
                                   , MenuEntry "XZ" (menuRotationPlane plane "XZ")
                                   , MenuEntry "XU" (menuRotationPlane plane "XU")
                                   , MenuEntry "YZ" (menuRotationPlane plane "YZ")
                                   , MenuEntry "YU" (menuRotationPlane plane "YU")
                                   , MenuEntry "ZU" (menuRotationPlane plane "ZU")
                                   ])
                   ])
  mainLoop


