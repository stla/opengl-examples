module Utils.SphereMesh
  (sphereMesh)
  where
import           Data.Tuple.Extra             ((&&&))
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..))
import           Linear                       (V3 (..), cross, signorm, (^+^),
                                               (^-^), (^/))

type TriangleV3 = (V3 Double, V3 Double, V3 Double)
type TriangleVx3 = (Vertex3 Double, Vertex3 Double, Vertex3 Double)

splitTriangleV3 :: TriangleV3 -> [TriangleV3]
splitTriangleV3 (a, b, c) = [tr1, tr2, tr3, tr4]
  where
    mab = signorm $ (a ^+^ b) ^/ 2
    mac = signorm $ (a ^+^ c) ^/ 2
    mbc = signorm $ (b ^+^ c) ^/ 2
    tr1 = (a, mab, mac)
    tr2 = (b, mbc, mab)
    tr3 = (c, mac, mbc)
    tr4 = (mab, mbc, mac)

icosahedron :: [TriangleV3]
icosahedron =
  map (toTriangle . map (map (/ sqrt (1+phi*phi))))
     [ [ [ 0.0 , 1.0 , -phi ]
       , [ -phi , 0.0 , -1.0 ]
       , [ -1.0 , phi , 0.0 ]
       ] -- 1
     , [ [ 0.0 , -1.0 , phi ]
       , [ -1.0 , -phi , 0.0 ]
       , [ 1.0 , -phi , 0.0 ]
       ] -- 2
     , [ [ 0.0 , -1.0 , phi ]
       , [ -phi , 0.0 , 1.0 ]
       , [ -1.0 , -phi , 0.0 ]
       ] -- 3
     , [ [ -phi , 0.0 , -1.0 ]
       , [ -phi , 0.0 , 1.0 ]
       , [ -1.0 , phi , 0.0 ]
       ] -- 4
     , [ [ -phi , 0.0 , -1.0 ]
       , [ -1.0 , -phi , 0.0 ]
       , [ -phi , 0.0 , 1.0 ]
       ] -- 5
     , [ [ 0.0 , 1.0 , phi ]
       , [ -1.0 , phi , 0.0 ]
       , [ -phi , 0.0 , 1.0 ]
       ] -- 6
     , [ [ 0.0 , -1.0 , phi ]
       , [ 0.0 , 1.0 , phi ]
       , [ -phi , 0.0 , 1.0 ]
       ] -- 7
     , [ [ 0.0 , 1.0 , -phi ]
       , [ 1.0 , phi , 0.0 ]
       , [ phi , 0.0 , -1.0 ]
       ] -- 8
     , [ [ 0.0 , 1.0 , phi ]
       , [ 1.0 , phi , 0.0 ]
       , [ -1.0 , phi , 0.0 ]
       ] -- 9
     , [ [ 0.0 , 1.0 , -phi ]
       , [ -1.0 , phi , 0.0 ]
       , [ 1.0 , phi , 0.0 ]
       ] -- 10
     , [ [ 0.0 , -1.0 , -phi ]
       , [ -1.0 , -phi , 0.0 ]
       , [ -phi , 0.0 , -1.0 ]
       ] -- 11
     , [ [ 0.0 , -1.0 , -phi ]
       , [ 0.0 , 1.0 , -phi ]
       , [ phi , 0.0 , -1.0 ]
       ] -- 12
     , [ [ 0.0 , -1.0 , -phi ]
       , [ -phi , 0.0 , -1.0 ]
       , [ 0.0 , 1.0 , -phi ]
       ] -- 13
     , [ [ 0.0 , -1.0 , -phi ]
       , [ phi , 0.0 , -1.0 ]
       , [ 1.0 , -phi , 0.0 ]
       ] -- 14
     , [ [ 0.0 , -1.0 , -phi ]
       , [ 1.0 , -phi , 0.0 ]
       , [ -1.0 , -phi , 0.0 ]
       ] -- 15
     , [ [ 0.0 , -1.0 , phi ]
       , [ phi , 0.0 , 1.0 ]
       , [ 0.0 , 1.0 , phi ]
       ] -- 16
     , [ [ 0.0 , -1.0 , phi ]
       , [ 1.0 , -phi , 0.0 ]
       , [ phi , 0.0 , 1.0 ]
       ] -- 17
     , [ [ phi , 0.0 , -1.0 ]
       , [ phi , 0.0 , 1.0 ]
       , [ 1.0 , -phi , 0.0 ]
       ]
     , [ [ phi , 0.0 , -1.0 ]
       , [ 1.0 , phi , 0.0 ]
       , [ phi , 0.0 , 1.0 ]
       ]
     , [ [ 0.0 , 1.0 , phi ]
       , [ phi , 0.0 , 1.0 ]
       , [ 1.0 , phi , 0.0 ]
       ]
     ]

  where
    phi = (1 + sqrt 5) / 2
    toTriangle x = (toV3 (x!!0), toV3 (x!!1), toV3 (x!!2))
    toV3 y = V3 (y!!0) (y!!1) (y!!2)

sphereMesh' :: Int -> [TriangleV3]
sphereMesh' n = iterate (concatMap splitTriangleV3) icosahedron !! n

sphereMesh :: Int -> [(TriangleVx3, Normal3 Double)]
sphereMesh n = map (toTriangleVx3 &&& normal) trianglesV3
  where
    trianglesV3 = sphereMesh' n
    toTriangleVx3 (a, b, c) = (v3toVx3 a, v3toVx3 b, v3toVx3 c)
    v3toVx3 (V3 x y z) = Vertex3 x y z
    normal (a, b, c) = v3toN $ signorm $ cross (b ^-^ a) (c ^-^ a)
    v3toN (V3 x y z) = Normal3 x y z
