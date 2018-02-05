module Utils.PrismaticPath
  where
import           Data.Tuple.Extra             ((&&&))
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..))
import           Linear                       (Conjugate, Epsilon, V3 (..),
                                               axisAngle, cross, norm, rotate,
                                               signorm, (^*), (^+^), (^-^))

prismaticPath :: (Floating a, RealFloat a, Epsilon a, Conjugate a, Enum a)
      => [Vertex3 a] -> Int -> a
      -> [(((Vertex3 a, Vertex3 a, Vertex3 a, Vertex3 a), Normal3 a), [((Vertex3 a, Vertex3 a), Normal3 a)])]
prismaticPath vs nsides radius =
  map ((f &&& g) &&& fg) [0 .. nsides-1]
  where
    nvertices = length vs
    axes = zipWith vector (init vs) (tail vs)
    pts0 = map (\a -> rotation (axes!!0) a firstpoint ^+^ vx3toV3 (vs!!0))
               [realToFrac i * 2 * pi / nsides' | i <- [0 .. nsides]]
    nsides' = realToFrac nsides
    firstpoint = n (vs!!0) (vs!!1) radius
    pts = foldr (\ax ps -> ps ++ [map (^+^ ax) (last ps)]) [pts0] axes
    pts1 = pts!!1
    f i = (v3toVx3 $ pts0!!i, v3toVx3 $ pts0!!(i+1), v3toVx3 $ pts1!!i,
           v3toVx3 $ pts1!!(i+1))
    g i = v3toN $ signorm $ cross (pts0!!(i+1) ^-^ pts0!!i)
                                  (pts1!!i ^-^ pts0!!i)
    pts2 = pts!!2
    -- f' i = (v3toVx3 $ pts2!!i, v3toVx3 $ pts2!!(i+1))
    -- g' i = v3toN $ signorm $ cross (pts1!!(i+1) ^-^ pts1!!i) (pts2!!i ^-^ pts1!!i)
    f' i = map (\p -> (v3toVx3 $ p!!i, v3toVx3 $ p!!(i+1))) (drop 2 pts)
    g' i = zipWith (\p1 p2 -> v3toN $ signorm $ cross (p1!!(i+1) ^-^ p1!!i) (p2!!i ^-^ p1!!i))
                   (init $ tail pts) (tail $ tail pts)
    fg i = zip (f' i) (g' i)
    rotation :: (Floating a, RealFloat a, Epsilon a, Conjugate a)
             => V3 a -> a -> V3 a -> V3 a
    rotation ax angle = rotate (axisAngle ax angle)
    v3toVx3 :: V3 a -> Vertex3 a
    v3toVx3 (V3 x y z) = Vertex3 x y z
    vx3toV3 :: Vertex3 a -> V3 a
    vx3toV3 (Vertex3 x y z) = V3 x y z
    vector :: Floating a => Vertex3 a -> Vertex3 a -> V3 a
    vector (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) = V3 (x2-x1) (y2-y1) (z2-z1)
    n :: (Floating a, Eq a) => Vertex3 a -> Vertex3 a -> a -> V3 a
    n (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) r = vec ^* (r / norm vec)
      where
        vec = if x1==x2 then V3 0 (z2-z1) (y1-y2) else V3 (y2-y1) (x1-x2) 0
    v3toN :: V3 a -> Normal3 a
    v3toN (V3 x y z) = Normal3 x y z
