module Utils.ConeDouble
  (cone)
  where
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..))
import           Utils.OpenGL                 (triangleNormal)

type Vrtx = (Double, Double, Double)
type Triangle = (Vrtx, Vrtx, Vrtx)
type Vx3d = Vertex3 Double
type TriangleVx3d = (Vx3d, Vx3d, Vx3d)

frac :: Int -> Int -> Double
frac p q = realToFrac p / realToFrac q

cone_ :: Int -> Double -> Double -> [Triangle]
cone_ n radius height = triangles1 ++ triangles2 ++ triangles3 ++ triangles4
  where
  circle = [(radius * cos (2 * pi * frac i n),
             radius * sin (2 * pi * frac i n),
             height) | i <- [0 .. n]]
  o = (0, 0, 0)
  triangles1 = zipWith (\p1 p2 -> (o,p1,p2)) (init circle) (tail circle)
  h = (0, 0, height)
  triangles2 = zipWith (\p1 p2 -> (p2,p1,h)) (init circle) (tail circle)
  triangles3 = zipWith (\p1 p2 -> (negate' p2, negate' p1, negate' o)) (init circle) (tail circle)
  triangles4 = zipWith (\p1 p2 -> (negate' h, negate' p1, negate' p2)) (init circle) (tail circle)
  negate' (x,y,z) = (-x,-y,-z)
cone :: Int -> Double -> Double -> [(TriangleVx3d, Normal3 Double)]
cone n radius height = zip triangles' normals
  where
  triangles = cone_ n radius height
  triangles' = map fromTriangle triangles
    where
    fromTriangle (a,b,c) = (toVx3d c, toVx3d b, toVx3d a)
    toVx3d (x,y,z) = Vertex3 x y z
  normals = map triangleNormal triangles'
