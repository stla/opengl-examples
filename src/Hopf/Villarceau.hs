module Hopf.Villarceau
  where
import           Control.Monad                     (when)
import qualified Data.ByteString                   as B
import           Data.IORef
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Hopf.Hopf
import           Linear                            hiding (lookAt, perspective,
                                                    rotate)
import           Text.Printf
import           Utils.CircleTransfo               (transformationMatrix)

data Context = Context
    {
      contextRot1 :: IORef GLfloat
    , contextRot2 :: IORef GLfloat
    , contextRot3 :: IORef GLfloat
    , contextZoom :: IORef Double
    }

white,black,red,green,blue :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1
red        = Color4    1    0    0    1
green      = Color4    0    1    0    0.5
blue       = Color4    0    0    1    0.5

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [(0,5,0),(0,5,12),(3,5,11)]

myPointsV3 :: [V3 GLfloat]
myPointsV3 = map toV3 myPoints
  where
    toV3 (x,y,z) = V3 x y z


-- transformationMatrix' :: V3 GLfloat -> V3 GLfloat -> V3 GLfloat -> IO ()
-- transformationMatrix' p1 p2 p3 = do
--   m <- newMatrix RowMajor tmat :: IO (GLmatrix GLfloat)
--   multMatrix m
--   where
--     tmat = transformationMatrix plane
--     (plane,_) = plane3pts p1 p2 p3

tmatAndRadius :: ([GLfloat], GLdouble)
tmatAndRadius = transformationMatrix
                (myPointsV3!!0) (myPointsV3!!1) (myPointsV3!!2)

transfo :: IO ()
transfo = do
  m <- newMatrix RowMajor (fst tmatAndRadius) :: IO (GLmatrix GLfloat)
  multMatrix m

radius :: GLdouble
radius = snd tmatAndRadius

display :: Context -> DisplayCallback
display context = do
  putStrLn "radius:"
  print radius
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  zoom <- get (contextZoom context)
  (_, size) <- get viewport
  loadIdentity
  resize zoom size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  preservingMatrix $ do
    transfo
    materialDiffuse FrontAndBack $= blue
    renderObject Solid $ Torus 0.1 radius 30 30
  renderPrimitive Triangles $ do
    materialDiffuse Front $= red
    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
  -- preservingMatrix $ do
  --   materialDiffuse FrontAndBack $= green
  --   renderObject Solid $ Torus 0.1 2 30 30
  swapBuffers

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-25+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h


keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef GLdouble -- zoom
         -> IORef Bool     -- animation
         -> KeyboardCallback
keyboard rot1 rot2 rot3 zoom anim c _ = do
  case c of
    'a' -> writeIORef anim True
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


main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Villarceau Circles"
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
  blend $= Enabled    -- allow transparency
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  anim <- newIORef False
  snapshots <- newIORef 0
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextZoom = zoom }
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 zoom anim)
  idleCallback $= Nothing -- Just (idle anim a n m quadsRef snapshots)
  putStrLn "*** Villarceau Circles ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Animation: a \n\
        \"
  mainLoop
