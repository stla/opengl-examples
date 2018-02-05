module Utils.Mobius
  (mobiusStrip, mobiusCurve)
  where
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..))
import           Linear                       (V3 (..), signorm, cross, (^-^))

type Rectangle = (V3 Double, V3 Double, V3 Double, V3 Double)
type Normal = V3 Double
type Vx3d = Vertex3 Double

intToDbl :: Int -> Double
intToDbl = realToFrac

mobiusStrip' :: Double -> Double -> [(Rectangle, Normal)]
mobiusStrip' hwidth radius =
  zip rectangles normals
  where
    n = 400
    u_ = [intToDbl i / intToDbl n * 4 * pi | i <- [0 .. n]] -- go to 4pi for the coloring
    c1 = map (\u -> V3 (cos u * (radius - hwidth * cos(u/2)))
                       (sin u * (radius - hwidth * cos(u/2)))
                       (-hwidth * sin(u/2))) u_
    c2 = map (\u -> V3 (cos u * (radius + hwidth * cos(u/2)))
                       (sin u * (radius + hwidth * cos(u/2)))
                       (hwidth * sin(u/2))) u_
    rectangles = map (\i -> (c1!!i, c2!!i, c2!!(i+1), c1!!(i+1))) [0 .. n-1]
    normals = map (\i -> normal (c1!!i) (c2!!i) (c2!!(i+1))) [0 .. n-1]
    normal v1 v2 v3 = signorm $ cross (v2^-^v1) (v3^-^v1)

mobiusStrip :: Double -> Double -> [((Vx3d, Vx3d, Vx3d, Vx3d), Normal3 Double)]
mobiusStrip hw r = map f (mobiusStrip' hw r)
  where
    f ((v1, v2, v3, v4), n) =
      ((v3toVx3 v1, v3toVx3 v2, v3toVx3 v3, v3toVx3 v4), v3toN n)
    v3toVx3 :: V3 a -> Vertex3 a
    v3toVx3 (V3 x y z) = Vertex3 x y z
    v3toN :: V3 a -> Normal3 a
    v3toN (V3 x y z) = Normal3 x y z

mobiusCurve :: Double -> Double -> Int -> [Vx3d]
mobiusCurve hwidth radius n =
  c1
  where
    u_ = [intToDbl i / intToDbl n * 4 * pi | i <- [0 .. n]]
    c1 = map (\u -> Vertex3 (cos u * (radius - hwidth * cos(u/2)))
                            (sin u * (radius - hwidth * cos(u/2)))
                            (-hwidth * sin(u/2))) u_
    -- c2 = map (\u -> Vertex3 (cos u * (radius + hwidth * cos(u/2)))
    --                         (sin u * (radius + hwidth * cos(u/2)))
    --                         (hwidth * sin(u/2))) u_
