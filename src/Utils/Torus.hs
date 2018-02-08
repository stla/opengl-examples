module Utils.Torus
  (torus, torus')
  where
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..))
import           Linear                       (V3 (..), signorm, cross, (^-^))

type Rectangle = (V3 Double, V3 Double, V3 Double, V3 Double)
type Normal = V3 Double
type Vx3d = Vertex3 Double

intToDbl :: Int -> Double
intToDbl = realToFrac

rotatey :: Double -> V3 Double -> V3 Double
rotatey theta (V3 x y z) =
  V3 (costheta*x + sintheta*z) y (-sintheta*x + costheta*z)
  where
    costheta = cos theta
    sintheta = sin theta

rotateRN :: Double -> (Rectangle, Normal) -> (Rectangle, Normal)
rotateRN a ((v1,v2,v3,v4),n) =
  ((rotatey a v1, rotatey a v2, rotatey a v3, rotatey a v4), rotatey a n)

_torus :: Double -> Double -> Int -> Int -> [(Rectangle, Normal)]
_torus holeRadius tubeRadius nradial ntubular =
  let rn = zip rectangles normals in
  rn ++ concatMap (\a -> map (rotateRN a) rn)
                 [intToDbl i * theta | i <- [1 .. nradial-1]]
  where
    r = holeRadius + tubeRadius
    c1 = map (\a -> V3 (r + tubeRadius * cos a) (tubeRadius * sin a) 0)
             [intToDbl i * 2 * pi / intToDbl ntubular | i <- [0 .. ntubular]]
    theta = 2 * pi / intToDbl nradial
    c2 = map (rotatey theta) c1
    rectangles = map (\i -> (c1!!i, c2!!i, c2!!(i+1), c1!!(i+1)))
                     [0 .. ntubular-1]
    normals = map (\i -> normal (c1!!i) (c2!!i) (c2!!(i+1)))
                  [0 .. ntubular-1]
    normal v1 v2 v3 = signorm $ cross (v2^-^v1) (v3^-^v1)

torus :: Double -> Double -> Int -> Int
      -> [((Vx3d, Vx3d, Vx3d, Vx3d), Normal3 Double)]
torus hr tr n1 n2 = map f (_torus hr tr n1 n2)
  where
    f ((v1, v2, v3, v4), n) =
      ((v3toVx3 v1, v3toVx3 v2, v3toVx3 v3, v3toVx3 v4), v3toN n)
    v3toVx3 :: V3 a -> Vertex3 a
    v3toVx3 (V3 x y z) = Vertex3 x y z
    v3toN :: V3 a -> Normal3 a
    v3toN (V3 x y z) = Normal3 x y z

-- QuadStrip
type Segment = (V3 Double, V3 Double)

rotateSN :: Double -> (Segment, Normal) -> (Segment, Normal)
rotateSN a ((v1,v2),n) =
  ((rotatey a v1, rotatey a v2), rotatey a n)

_torus' :: Double -> Double -> Int -> Int
        -> [((Rectangle, Normal), [(Segment, Normal)])]
_torus' holeRadius tubeRadius nradial ntubular =
  zip rns snss
  where
    r = holeRadius + tubeRadius
    c1 = map (\a -> V3 (r + tubeRadius * cos a) (tubeRadius * sin a) 0)
             [intToDbl i * 2 * pi / intToDbl ntubular | i <- [0 .. ntubular]]
    theta = 2 * pi / intToDbl nradial
    c2 = map (rotatey theta) c1
    rectangle = (c1!!0, c2!!0, c1!!1, c2!!1)
    segments = map (\i -> if True then (c1!!(i+1), c2!!(i+1)) else (c2!!(i+1), c1!!(i+1)))
                     [1 .. ntubular-1]
    normals = map (\i -> normal (c1!!i) (c2!!i) (c2!!(i+1)))
                  [0 .. ntubular-1]
    normal v1 v2 v3 = signorm $ cross (v2^-^v1) (v3^-^v1)
    rn1 = (rectangle, head normals)
    rns = rn1 : map (\a -> rotateRN a rn1)
                    [intToDbl i * theta | i <- [1 .. nradial-1]]
    sns1 = zip segments (tail normals)
    snss = sns1 : map (\a -> map (rotateSN a) sns1)
                  [intToDbl i * theta | i <- [1 .. nradial-1]]

torus' :: Double -> Double -> Int -> Int
       -> [(((Vx3d, Vx3d, Vx3d, Vx3d), Normal3 Double),
            [((Vx3d, Vx3d), Normal3 Double)])]
torus' hr tr n1 n2 = map f (_torus' hr tr n1 n2)
  where
    f (((v1, v2, v3, v4), n), l) =
      (((v3toVx3 v1, v3toVx3 v2, v3toVx3 v3, v3toVx3 v4), v3toN n), map g l)
    g ((v1,v2), n) = ((v3toVx3 v1, v3toVx3 v2), v3toN n)
    v3toVx3 :: V3 a -> Vertex3 a
    v3toVx3 (V3 x y z) = Vertex3 x y z
    v3toN :: V3 a -> Normal3 a
    v3toN (V3 x y z) = Normal3 x y z
