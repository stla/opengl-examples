module Utils.Prism
  where
import           Data.Tuple.Extra             ((&&&))
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..))
import           Linear                       (Conjugate, Epsilon, V3 (..))
import qualified Linear                       as L

prism :: (Floating a, RealFloat a, Epsilon a, Conjugate a, Enum a)
      => Vertex3 a -> Vertex3 a -> Int -> a
      -> [((Vertex3 a, Vertex3 a, Vertex3 a, Vertex3 a), Normal3 a)]
prism v1 v2 nsides radius =
  map (f &&& g) [0 .. nsides-1]
  where
    axis = vector v1 v2
    pts = map (\a -> rotation axis a firstpoint L.^+^ vx3toV3 v1)
              [realToFrac i * 2 * pi / nsides' | i <- [0 .. nsides]]
    nsides' = realToFrac nsides
    firstpoint = n v1 v2 radius
    rotation :: (Floating a, RealFloat a, Epsilon a, Conjugate a)
             => V3 a -> a -> V3 a -> V3 a
    rotation ax angle = L.rotate (L.axisAngle ax angle)
    v3toVx3 :: V3 a -> Vertex3 a
    v3toVx3 (V3 x y z) = Vertex3 x y z
    vx3toV3 :: Vertex3 a -> V3 a
    vx3toV3 (Vertex3 x y z) = V3 x y z
    vector :: Floating a => Vertex3 a -> Vertex3 a -> V3 a
    vector (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) = V3 (x2-x1) (y2-y1) (z2-z1)
    n :: (Floating a, Eq a) => Vertex3 a -> Vertex3 a -> a -> V3 a
    n (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) r = vec L.^* (r / L.norm vec)
      where
        vec = if x1==x2 then V3 0 (z2-z1) (y1-y2) else V3 (y2-y1) (x1-x2) 0
    pts' = map (L.^+^ axis) pts
    f i = (v3toVx3 $ pts!!i, v3toVx3 $ pts'!!i,
           v3toVx3 $ pts'!!(i+1), v3toVx3 $ pts!!(i+1))
    g i = v3toN $ L.signorm $ L.cross (pts'!!i L.^-^ pts!!i)
                                      (pts!!(i+1) L.^-^ pts!!i)
    v3toN :: V3 a -> Normal3 a
    v3toN (V3 x y z) = Normal3 x y z
