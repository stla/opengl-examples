-- http://paulbourke.net/geometry/klein/
module KleinBottleGray.Data
  (allQuads, Quad)
  where
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as M
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..))
import           Utils.OpenGL                 (triangleNormal)

type Quad = ((Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double), Normal3 Double)

function :: Double -> Double -> Double -- parameters a, n, m
         -> Double -> Double -- variables u and v
         -> [Double]         -- result vector
function a n m u v = [x, y, z]
  where
    x = (a + cos(n*u/2.0) * sin(v) - sin(n*u/2.0) * sin(2*v)) * cos(m*u/2.0)
    y = (a + cos(n*u/2.0) * sin(v) - sin(n*u/2.0) * sin(2*v)) * sin(m*u/2.0)
    z = sin(n*u/2.0) * sin(v) + cos(n*u/2.0) * sin(2*v)

quad :: (Double -> Double -> Double -> Double -> Double -> [Double]) -- the function
     -> Double -> Double -> Double               -- parameters a, n, m
     -> [Double] -> [Double]                     -- sequences of u and v
     -> Int -> Int                               -- indices
     -> Quad
quad f a n m u_ v_ i j = ((x, y, z, t), norm)
  where
  (x,y,z,t) = ( toVx3 $ f a n m (u_!!i) (v_!!j)
              , toVx3 $ f a n m (u_!!i) (v_!!(j+1))
              , toVx3 $ f a n m (u_!!(i+1)) (v_!!(j+1))
              , toVx3 $ f a n m (u_!!(i+1)) (v_!!j)    )
  norm = triangleNormal (x, y, z)
  toVx3 w = Vertex3 (w!!0) (w!!1) (w!!2)

allQuads :: Int -> Int -- numbers of subdivisions for u and v
         -> Double -> Double -> Double -- parameters a, n, m
         -> Map (Int,Int) Quad
allQuads n_u n_v a n m =
    M.fromList (zip indices (map (uncurry (quad function a n m sequ seqv)) indices))
    where
      indices = [(i,j) | i <- [0 .. n_u-1], j <- [0 .. n_v-1]]
      sequ,seqv :: [Double]
      sequ = [4*pi * frac i n_u | i <- [0 .. n_u]]
      seqv = [4*pi * frac i n_v | i <- [0 .. n_v]]
      frac :: Int -> Int -> Double
      frac p q = realToFrac p / realToFrac q


