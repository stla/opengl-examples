module Cone.TwentyCones
  where
import           Data.Vector     (Vector, (!))
import           Cone.ConeMesh
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Control.Monad                (forM_)
import           Linear                       (V3 (..))

type Mesh = (Vector ((Float,Float,Float),(Float,Float,Float)), [(Int,Int,Int,Int)])

white,black,blue :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1
blue       = Color4    0    0    1    1

toVx3 :: (Float,Float,Float) -> Vertex3 Float
toVx3 (x,y,z) = Vertex3 x y z

toN3 :: (Float,Float,Float) -> Normal3 Float
toN3 (x,y,z) = Normal3 x y z

phi,a,b,c :: GLfloat
phi = (1 + sqrt 5) / 2
a = 1 / sqrt 3
b = a / phi
c = a * phi

points :: [V3 GLfloat]
points = 
   [V3 a a a,
    V3 a a (-a),
    V3 a (-a) a,
    V3 (-a) (-a) a,
    V3 (-a) a (-a),
    V3 (-a) a a,
    V3 0 b (-c),
    V3 0 (-b) (-c),
    V3 0 (-b) c,
    V3 c 0 (-b),
    V3 (-c) 0 (-b),
    V3 (-c) 0 b,
    V3 b c 0,
    V3 b (-c) 0,
    V3 (-b) (-c) 0,
    V3 (-b) c 0,
    V3 0 b c,
    V3 a (-a) (-a),
    V3 c 0 b,
    V3 (-a) (-a) (-a)]


meshesAndMatrices :: [(Mesh, [Float])]
meshesAndMatrices = 
    map (\pt -> coneMesh pt (V3 0 0 0) 0.1 0.03 3 32) points

-- mesh :: Mesh
-- mesh = fst meshAndMatrix

-- verticesAndNormals :: Vector ((Float,Float,Float),(Float,Float,Float))
-- verticesAndNormals = fst mesh

-- quadIndices :: [(Int,Int,Int,Int)]
-- quadIndices = snd mesh

-- transfoMatrix :: [Float]
-- transfoMatrix = snd meshAndMatix

display :: DisplayCallback
display = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  forM_ meshesAndMatrices $ \meshAndMatrix -> 
    preservingMatrix $ do
      m <- newMatrix RowMajor (snd meshAndMatrix) :: IO (GLmatrix GLfloat)
      multMatrix m
      forM_ ((snd . fst) meshAndMatrix) $ \(i,j,k,l) ->
        renderPrimitive Quads $ do
          materialDiffuse Front $= blue
          drawQuad i j k l ((fst . fst) meshAndMatrix)
  swapBuffers
  where
    drawQuad i j k l verticesAndNormals = do 
      normal ni
      vertex vi
      normal nj
      vertex vj
      normal nk
      vertex vk
      normal nl
      vertex vl
      where
        (vi',ni') = verticesAndNormals ! i
        (vj',nj') = verticesAndNormals ! j
        (vk',nk') = verticesAndNormals ! k
        (vl',nl') = verticesAndNormals ! l
        vi = toVx3 vi'
        ni = toN3 ni'
        vj = toVx3 vj'
        nj = toN3 nj'
        vk = toVx3 vk'
        nk = toN3 nk'
        vl = toVx3 vl'
        nl = toN3 nl'

resize :: Size -> IO ()
resize s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 2 2 3) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Twenty cones"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient Front $= black
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 200 200 300 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= black
  depthFunc $= Just Less
  shadeModel $= Smooth
  displayCallback $= display
  reshapeCallback $= Just resize
  idleCallback $= Nothing
  mainLoop