module BoyRoman.Data where
import           Data.List
import           Graphics.Rendering.OpenGL.GL (GLdouble, Normal3 (..),
                                               Vertex3 (..))
import           Utils.OpenGL                 (triangleNormal)

boyRoman :: Double -> Double -> Double -> [Double]
boyRoman alpha u v = [x, y, z]
  where
    x = ( sqrt 2 * cos (2*u) *  cos v ^^2 + cos  u * sin (2*v) ) /
         ( 2 - alpha * sqrt 2 * sin (3*u) * sin (2*v) )
    y = ( sqrt 2 * sin (2*u) *  cos v ^^2 + sin  u * sin (2*v) ) /
         ( 2 - alpha * sqrt 2 * sin (3*u) * sin (2*v) )
    z = 3 * cos v ^^2 /
         ( 2 - alpha * sqrt 2 * sin (3*u) * sin (2*v) )

boyRoman' :: Double -> Double -> Double -> Vertex3 Double
boyRoman' alpha u v = toVx3 $ boyRoman alpha u v
  where
    toVx3 x = Vertex3 (x!!0) (x!!1) (x!!2)

quad :: (Double -> Double -> Double -> Vertex3 Double) -> Double -> [Double]
     -> [Double] -> Int -> Int
     -> ((Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double),
          Normal3 Double)
quad f alpha u_ v_ i j = ((a, b, c, d), norm)
  where
  (a,b,c,d) = ( f alpha (u_!!i) (v_!!j)
              , f alpha (u_!!i) (v_!!(j+1))
              , f alpha (u_!!(i+1)) (v_!!(j+1))
              , f alpha (u_!!(i+1)) (v_!!j) )
  norm = triangleNormal (a, b, c)

allQuads :: Int -> Double
         -> [((Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double),
               Normal3 Double)]
allQuads n alpha = map (uncurry (quad boyRoman' alpha sequ seqv))
                   [(i,j) | i <- [0 .. length sequ - 2], j <- [0 .. length seqv - 2]]
        where
            sequ,seqv :: [Double]
            sequ = [pi/2 * (2 * frac i n - 1) | i <- [0 .. n]]
            seqv = [pi * frac i n | i <- [0 .. n]]
            frac :: Int -> Int -> Double
            frac p q = realToFrac p / realToFrac q

