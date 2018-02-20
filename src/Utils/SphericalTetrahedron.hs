module Utils.SphericalTetrahedron
  (stMesh, stMesh')
  where
import Data.Tuple.Extra ((&&&))
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..))
import           Linear                       (V3 (..), cross, signorm,
                                               (^+^), (^-^), (^/))

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

-- spherical to Cartesian - unit sphere
s2c :: (Double, Double) -> V3 Double
s2c (theta, phi) = V3 (cos theta * sin phi) (sin theta * sin phi) (cos phi)

stMesh_ :: Int -> (Double,Double) -> (Double,Double) -> (Double,Double)
        -> [TriangleV3]
stMesh_ n a b c = iterate (concatMap splitTriangleV3) [striangle] !! n
  where
    aa = s2c a
    bb = s2c b
    cc = s2c c
    striangle = (aa, bb, cc)

stMesh :: Int -> (Double,Double) -> (Double,Double) -> (Double,Double)
       -> [(TriangleVx3, Normal3 Double)]
stMesh n a b c = map (toTriangleVx3 &&& normal) trianglesV3
  where
    trianglesV3 = stMesh_ n a b c
    toTriangleVx3 (a, b, c) = (v3toVx3 a, v3toVx3 b, v3toVx3 c)
    v3toVx3 (V3 x y z) = Vertex3 x y z
    normal (a, b, c) = v3toN $ signorm $ cross (c ^-^ a) (b ^-^ a)
    v3toN (V3 x y z) = Normal3 x y z

-- spherical to Cartesian - arbitrary radius
s2c' :: (Double, (Double, Double)) -> V3 Double
s2c' (r, (theta, phi)) = V3 (r * cos theta * sin phi)
                            (r * sin theta * sin phi)
                            (r * cos phi)

stMesh_' :: Int -> Double -> (Double,Double) -> (Double,Double) -> (Double,Double)
        -> [TriangleV3]
stMesh_' n radius a b c = iterate (concatMap splitTriangleV3) [striangle] !! n
  where
    aa = s2c' (radius, a)
    bb = s2c' (radius, b)
    cc = s2c' (radius, c)
    striangle = (aa, bb, cc)

stMesh' :: Int -> Double -> (Double,Double) -> (Double,Double) -> (Double,Double)
       -> [(TriangleVx3, Normal3 Double)]
stMesh' n radius a b c = map (toTriangleVx3 &&& normal) trianglesV3
  where
    trianglesV3 = stMesh_' n radius a b c
    toTriangleVx3 (a, b, c) = (v3toVx3 a, v3toVx3 b, v3toVx3 c)
    v3toVx3 (V3 x y z) = Vertex3 x y z
    normal (a, b, c) = v3toN $ signorm $ cross (c ^-^ a) (b ^-^ a)
    v3toN (V3 x y z) = Normal3 x y z
