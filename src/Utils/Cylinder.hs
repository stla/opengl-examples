module Utils.Cylinder
  where
import           Graphics.Rendering.OpenGL.GL (Vertex3 (..))
import           Linear                       (Conjugate, Epsilon, V3 (..))
import qualified Linear                       as L

tstripCylinder :: (Floating a, RealFloat a, Epsilon a, Conjugate a, Enum a) =>
                  Vertex3 a -> Vertex3 a -> a -> [Vertex3 a]
tstripCylinder v1 v2 radius =
  map v3toVertex3 (concatMap (\p -> [p, p L.^+^ axis]) points)
  where
    axis = vector v1 v2
    points = map (\a -> rotation axis a firstpoint)
                 [i * pi / 180 | i <- [0 .. 360]]
    firstpoint = n v1 v2 radius L.^+^ vertex3toV3 v1
    rotation :: (Floating a, RealFloat a, Epsilon a, Conjugate a)
             => V3 a -> a -> V3 a -> V3 a
    rotation ax angle = L.rotate (L.axisAngle ax angle)
    v3toVertex3 :: V3 a -> Vertex3 a
    v3toVertex3 (V3 x y z) = Vertex3 x y z
    vertex3toV3 :: Vertex3 a -> V3 a
    vertex3toV3 (Vertex3 x y z) = V3 x y z
    vector :: Floating a => Vertex3 a -> Vertex3 a -> V3 a
    vector (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) = V3 (x2-x1) (y2-y1) (z2-z1)
    n :: Floating a => Vertex3 a -> Vertex3 a -> a -> V3 a
    n (Vertex3 x1 y1 _) (Vertex3 x2 y2 _) r = vec L.^* (r / norm)
      where
        x = x2 - x1
        y = y2 - y1
        vec = V3 y (-x) 0
        norm = sqrt (y*y + x*x)
