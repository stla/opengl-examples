module Cyclide.Data
  (allTriangles, NTriangle)
  where
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..))
import           Utils.OpenGL                 (triangleNormal)

type NTriangle = ((Vertex3 Double, Vertex3 Double, Vertex3 Double), Normal3 Double)

fun :: Double -> Double -> Double -> Double -> Double -> [Double]
fun a b d u v = [x, y, z]
  where
    c = sqrt(a*a-b*b)
    den = a-c*cos(u)*cos(v)
    x = (d*(c-a*cos(u)*cos(v))+b*b*cos(u)) / den
    y = (b*sin(u)*(a-d*cos(v))) / den
    z = (b*sin(v)*(c*cos(u)-d)) / den

fun' :: Double -> Double -> Double -> Double -> Double -> Vertex3 Double
fun' a b d u v = toVx3 $ fun a b d u v
  where
    toVx3 x = Vertex3 (x!!0) (x!!1) (x!!2)

triangle :: (Double -> Double -> Vertex3 Double)
         -> [Double] -> [Double] -> Int -> Int
         -> [NTriangle]
triangle f u_ v_ i j = [((a, b, c), normal1), ((c, b, d), normal2)]
  where
  (a,c,d,b) = ( f (u_!!i) (v_!!j)
              , f (u_!!i) (v_!!(j+1))
              , f (u_!!(i+1)) (v_!!(j+1))
              , f (u_!!(i+1)) (v_!!j) )
  normal1 = triangleNormal (a, b, c)
  normal2 = triangleNormal (c, b, d)

allTriangles :: Int -> Double -> Double -> Double -> [NTriangle]
allTriangles m a b d =
    concatMap (uncurry (triangle (fun' a b d) sequ seqv))
        [(i,j) | i <- [0 .. length sequ - 2], j <- [0 .. length seqv - 2]]
        where
            sequ,seqv :: [Double]
            sequ = [2*pi * frac i m | i <- [0 .. m]]
            seqv = [2*pi * frac i m | i <- [0 .. m]]
            frac :: Int -> Int -> Double
            frac p q = realToFrac p / realToFrac q
