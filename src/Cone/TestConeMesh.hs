module Cone.TestConeMesh
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

meshAndMatrix :: (Mesh, [Float])
meshAndMatrix = 
    coneMesh (V3 0 0 0) (V3 2 2 2) 0.5 1 3 16

mesh :: Mesh
mesh = fst meshAndMatrix

verticesAndNormals :: Vector ((Float,Float,Float),(Float,Float,Float))
verticesAndNormals = fst mesh

toVx3 :: (Float,Float,Float) -> Vertex3 Float
toVx3 (x,y,z) = Vertex3 x y z

toN3 :: (Float,Float,Float) -> Normal3 Float
toN3 (x,y,z) = Normal3 x y z

quadIndices :: [(Int,Int,Int,Int)]
quadIndices = snd mesh

transfoMatrix :: [Float]
transfoMatrix = snd meshAndMatrix

display :: DisplayCallback
display = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  preservingMatrix $ do
    m <- newMatrix RowMajor transfoMatrix :: IO (GLmatrix GLfloat)
    multMatrix m
    forM_ quadIndices $ \(i,j,k,l) ->
      renderPrimitive Quads $ do
        materialDiffuse Front $= blue
        drawQuad i j k l
  swapBuffers
  where
    drawQuad i j k l = do 
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
  lookAt (Vertex3 2 2 13) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Cone"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient Front $= black
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 200 200 1300 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= black
  depthFunc $= Just Less
  shadeModel $= Smooth
  displayCallback $= display
  reshapeCallback $= Just resize
  idleCallback $= Nothing
  mainLoop