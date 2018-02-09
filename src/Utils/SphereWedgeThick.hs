module Utils.SphereWedgeThick
  (sphereWedgeThick)
  where
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..))
import           Utils.OpenGL                 (triangleNormal, negateNormal)

type Quad = (Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double)
type Triangle = (Vertex3 Double, Vertex3 Double, Vertex3 Double)
type Wedge = ([(Triangle, Normal3 Double)], [(Quad, Normal3 Double)])

intToDbl :: Int -> Double
intToDbl = realToFrac

frac :: Int -> Int -> Double
frac p q = intToDbl p / intToDbl q

-- | spherical to Cartesian
s2c :: (Double, Double) -> Vertex3 Double
s2c (theta, phi) = Vertex3 (cos theta * sin phi) (sin theta * sin phi) (cos phi)

scaleVertex :: Double -> Vertex3 Double -> Vertex3 Double
scaleVertex s (Vertex3 x y z) = Vertex3 (s*x) (s*y) (s*z)

scaleTriangle :: Double -> Triangle -> Triangle
scaleTriangle s (v1,v2,v3) =
  (scaleVertex s v1, scaleVertex s v2, scaleVertex s v3)

scaleQuad :: Double -> Quad -> Quad
scaleQuad s (v1,v2,v3,v4) =
  (scaleVertex s v1, scaleVertex s v2, scaleVertex s v3, scaleVertex s v4)

sphereWedgeThick :: Double -> Double -> Double -> Int -> Int
                 -> ((Wedge, Wedge), [(Quad, Normal3 Double)])
sphereWedgeThick thickness theta1 theta2 nm np =
  (((zip triangles tnormals, zip quads qnormals),
    (zip triangles' tnormals', zip quads' qnormals')),
   zip (quads'' ++ quads''') (qnormals'' ++ qnormals'''))
  where
  thetas = [theta1 + (theta2-theta1) * frac i nm | i <- [0 .. nm]]
  phis = [pi * frac i np | i <- [1 .. np - 1]]
  parallels = map (\phi -> map (\theta -> s2c (theta,phi)) thetas) phis
  quads = concat $ zipWith rectangles (init parallels) (tail parallels)
    where
    rectangles p1 p2 = map (\i -> (p1!!i, p1!!(i+1), p2!!(i+1), p2!!i))
                           [0 .. length p1 - 2]
  qnormal :: Quad -> Normal3 Double
  qnormal (v1, v2, v3, _) = triangleNormal (v1, v2, v3)
  qnormals = map qnormal quads
  triangles = trigles north (head parallels) ++
              trigles south (reverse $ last parallels)
    where
      trigles p0 pts = map (\i -> (p0, pts!!i, pts!!(i+1)))
                            [0 .. length pts - 2]
      north = Vertex3 0 0 1
      south = Vertex3 0 0 (-1)
  tnormals' = map triangleNormal triangles
  tnormals = map negateNormal tnormals'
  quads' = map (scaleQuad (1+thickness)) quads
  qnormals' = map negateNormal qnormals
  triangles' = map (scaleTriangle (1+thickness)) triangles
  vs = Vertex3 (0::Double) 0 1 : map head parallels ++ [Vertex3 (0::Double) 0 (-1)]
  svs = map (scaleVertex (1+thickness)) vs
  quads'' = map (\i -> (vs!!i, vs!!(i+1), svs!!(i+1), svs!!i))
            [0 .. length vs - 2]
  qnormals'' = map qnormal quads''
  vs' = Vertex3 (0::Double) 0 1 : map last parallels ++ [Vertex3 (0::Double) 0 (-1)]
  svs' = map (scaleVertex (1+thickness)) vs'
  quads''' = map (\i -> (svs'!!i, svs'!!(i+1), vs'!!(i+1), vs'!!i))
            [0 .. length vs' - 2]
  qnormals''' = map qnormal quads'''
