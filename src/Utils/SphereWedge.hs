module Utils.SphereWedge
  (sphereWedge)
  where
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..))
import           Utils.OpenGL                 (triangleNormal)

type Quad = (Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double)
type Triangle = (Vertex3 Double, Vertex3 Double, Vertex3 Double)

intToDbl :: Int -> Double
intToDbl = realToFrac

frac :: Int -> Int -> Double
frac p q = intToDbl p / intToDbl q

-- spherical to Cartesian
s2c :: (Double, Double) -> Vertex3 Double
s2c (theta, phi) = Vertex3 (cos theta * sin phi) (sin theta * sin phi) (cos phi)

sphereWedge :: Double -> Double -> Int -> Int
            -> ([(Triangle, Normal3 Double)], [(Quad, Normal3 Double)])
sphereWedge theta1 theta2 nm np = (zip triangles tnormals, zip quads qnormals)
  where
  thetas = [theta1 + (theta2-theta1) * frac i nm | i <- [0 .. nm]]
  phis = [pi * frac i np | i <- [1 .. np - 1]]
  parallels = map (\phi -> map (\theta -> s2c (theta,phi)) thetas) phis
  quads = concat $ zipWith rectangles (init parallels) (tail parallels)
    where
    rectangles p1 p2 = map (\i -> (p1!!i, p1!!(i+1), p2!!(i+1), p2!!i))
                           [0 .. length p1 - 2]
  qnormals = map qnormal quads
    where
    qnormal (v1, v2, v3, _) = triangleNormal (v1, v3, v2)
  triangles = trigles north (head parallels) ++
              trigles south (reverse $ last parallels)
    where
      trigles p0 pts = map (\i -> (p0, pts!!i, pts!!(i+1)))
                            [0 .. length pts - 2]
      north = Vertex3 0 0 1
      south = Vertex3 0 0 (-1)
  tnormals = map triangleNormal triangles
