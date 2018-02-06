module Utils.HorseSaddle
  (horseSaddle)
  where
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..))

type T = (Double, Double, Double)
type Rectangle = (T, T, T, T)
type Normal = T
type RectVx = (Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double)

intToDbl :: Int -> Double
intToDbl = realToFrac

f :: Double -> Double -> Double
f x y = x*x - y*y

horseSaddle_ :: Int -> Int -> (([Rectangle], [Normal]), [[T]])
horseSaddle_ nx ny = ((rectangles, normals), pts ++ pts2)
  where
  subdx = [2 * intToDbl i / intToDbl nx - 1 | i <- [0 .. nx]]
  subdy = [2 * intToDbl i / intToDbl ny - 1 | i <- [0 .. ny]]
  pts = map (\x -> zip3 (replicate (ny+1) x) subdy (map (f x) subdy)) subdx
  pts2 = map (\y -> zip3 subdx (replicate (nx+1) y) (map (`f` y) subdx)) subdy
  rectangles = concatMap (\j ->
                          map (\i ->
                               (pts!!j!!i, pts!!j!!(i+1), pts!!(j+1)!!(i+1),
                                pts!!(j+1)!!i))
                              [0 .. ny-1])
                          [0 .. nx-1]
  normals = concatMap (\j ->
                        map (\i ->
                              normal (pts!!j!!i) (pts!!j!!(i+1)) (pts!!(j+1)!!(i+1)))
                            [0 .. ny-1])
                      [0 .. nx-1]
    where
    normal t1 t2 t3 = (a', b', c')
      where
      (x1, x2, x3) = t1
      (y1, y2, y3) = t2
      (z1, z2, z3) = t3
      (a, b, c) = crossProd (y1-x1, y2-x2, y3-x3) (z1-x1, z2-x2, z3-x3)
      crossProd (a1,a2,a3) (b1,b2,b3) = (a2*b3-a3*b2, a3*b1-a1*b3, a1*b2-a2*b1)
      norm = sqrt (a*a + b*b + c*c)
      a' = a / norm
      b' = b / norm
      c' = c / norm

horseSaddle :: Int -> Int -> ([(RectVx, Normal3 Double)], [[Vertex3 Double]])
horseSaddle nx ny = (zip rectangles' normals', ls')
  where
  ((rectangles, normals), ls) = horseSaddle_ nx ny
  rectangles' = map toRectVx rectangles
  ls' = map (map toVx) ls
  toRectVx (a, b, c, d) = (toVx a, toVx b, toVx c, toVx d)
  toVx (x, y, z) = Vertex3 x y z
  normals' = map toNormal3 normals
    where
    toNormal3 (x, y, z) = Normal3 x y z
