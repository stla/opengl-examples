module Dupin.Data where
import           Data.List
import           Graphics.Rendering.OpenGL.GL (GLdouble, Normal3 (..),
                                               Vertex3 (..))
import           Utils.OpenGL                 (triangleNormal)


dupin :: Double -> Double -> Double -> Double -> Double -> [Double]
dupin a b d u v = [x, y, z]
  where
    c = sqrt(a*a-b*b)
    x = (d*(c-a*cos(u)*cos(v)) + b^2*cos(u))/(a-c*cos(u)*cos(v))
    y = (b*sin(u)*(a-d*cos(v)))/(a-c*cos(u)*cos(v))
    z = (b*sin(v)*(c*cos(u)-d))/(a-c*cos(u)*cos(v))

dupin' :: Double -> Double -> Double -> Double -> Double -> Vertex3 Double
dupin' a b d u v = toVx3 $ dupin a b d u v
  where
    toVx3 x = Vertex3 (x!!0) (x!!1) (x!!2)

quad :: (Double -> Double -> Double -> Double -> Double -> Vertex3 Double)
     -> Double -> Double -> Double -> [Double]
     -> [Double] -> Int -> Int
     -> ((Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double),
        Normal3 Double)
quad f a b d u_ v_ i j = ((aa, bb, cc, dd), norm)
 where
 (aa,bb,cc,dd) = ( f a b d (u_!!i) (v_!!j)
             , f a b d (u_!!i) (v_!!(j+1))
             , f a b d (u_!!(i+1)) (v_!!(j+1))
             , f a b d (u_!!(i+1)) (v_!!j) )
 norm = triangleNormal (aa, bb, cc)

allQuads :: Int -> Double -> Double -> Double
         -> [((Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double),
              Normal3 Double)]
allQuads n a b d = map (uncurry (quad dupin' a b d sequ seqv))
                   [(i,j) | i <- [0 .. length sequ - 2], j <- [0 .. length seqv - 2]]
        where
            sequ,seqv :: [Double]
            sequ = [2 * pi * frac i n | i <- [0 .. n]]
            seqv = [2 * pi * frac i n | i <- [0 .. n]]
            frac :: Int -> Int -> Double
            frac p q = realToFrac p / realToFrac q

