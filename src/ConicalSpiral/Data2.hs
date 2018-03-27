module ConicalSpiral.Data2 where
-- import           Data.List
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..))
import           Utils.OpenGL                 (triangleNormal)

function :: Double           -- parameter n
         -> Double -> Double -- variables u and v
         -> [Double]         -- result vector
function n u v = [x, y, z]
  where
    x = sin(n*v+0.5*pi) * ((1-0.5*v/pi) * (1-cos u) - 0.2)
    y = 4*v/pi + (1-0.5*v/pi) * sin u
    z = cos(n*v+0.5*pi) * ((1-0.5*v/pi) * (1-cos u) + 0.2)

quad :: (Double -> Double -> Double -> [Double]) -- the function
     -> Double                                   -- parameter n
     -> [Double] -> [Double]                     -- sequences of u and v
     -> Int -> Int                               -- indices
     -> ((Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double),
          Normal3 Double)
quad f n u_ v_ i j = ((a, b, c, d), norm)
  where
  (a,b,c,d) = ( toVx3 $ f n (u_!!i) (v_!!j)
              , toVx3 $ f n (u_!!i) (v_!!(j+1))
              , toVx3 $ f n (u_!!(i+1)) (v_!!(j+1))
              , toVx3 $ f n (u_!!(i+1)) (v_!!j)    )
  norm = triangleNormal (a, b, c)
  toVx3 x = Vertex3 (x!!0) (x!!1) (x!!2)

allQuads :: Int -> Int -- numbers of subdivisions for u and v
         -> Double     -- parameter n
         -> [((Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double),
               Normal3 Double)]
allQuads n_u n_v n =
    map (uncurry (quad function n sequ seqv))
        [(i,j) | i <- [0 .. n_u-1], j <- [0 .. n_v-1]]
    where
      sequ,seqv :: [Double]
      sequ = [2*pi * frac i n_u | i <- [0 .. n_u]]
      seqv = [2*pi * frac i n_v | i <- [0 .. n_v]]
      frac :: Int -> Int -> Double
      frac p q = realToFrac p / realToFrac q

