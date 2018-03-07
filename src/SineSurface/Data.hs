module SineSurface.Data
  where
import           Data.Tuple.Extra             (both)
import           Graphics.Rendering.OpenGL.GL (GLdouble, Vertex3 (..))

sinesurface :: Double -> [[Double]]
sinesurface a =
--  map (\uu -> map (\vv -> [a * sin uu, a * sin vv, a * sin (uu+vv)]))
  map (\(u,v) -> [a * sin u, a * sin v, a * sin (u+v)])
      [(x,y) | x <- u, y <- v]
  -- zipWith (\uu vv ->
  -- [a * sin uu
  -- ,a * sin vv
  -- ,a * sin (uu+vv)]) u v
  where
    u = [2*pi* frac i n | i <- [0 .. n]]
    v = [2*pi* frac i n | i <- [0 .. n]]
    frac :: Int -> Int -> Double
    frac p q = realToFrac p / realToFrac q
    n = 100

sinesurface' :: Double -> [Vertex3 Double]
sinesurface' a = map toVx3 (sinesurface a)
  where
    toVx3 xyz = Vertex3 (xyz!!0) (xyz!!1) (xyz!!2)
