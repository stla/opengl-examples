module Seashell.Data
  (allQuads, Quad)
  where
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as M
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..))
import           Utils.OpenGL                 (triangleNormal)

type Quad = ((Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double), Normal3 Double)

function :: Double -> Double -- parameters h, n
         -> Double -> Double -- variables u and v
         -> [Double]         -- result vector
function h n u v = [x, y, z]
  where
    w = u/2/pi
    x = w * cos(n*u) * (1 + cos v)
    y = w * sin(n*u) * (1 + cos v)
    z = w * sin v + h * w*w

quad :: (Double -> Double -> Double -> Double -> [Double]) -- the function
     -> Double -> Double               -- parameters h, n
     -> [Double] -> [Double]                     -- sequences of u and v
     -> Int -> Int                               -- indices
     -> Quad
quad f h n u_ v_ i j = ((x, y, z, t), norm)
  where
  (x,y,z,t) = ( toVx3 $ f h n (u_!!i) (v_!!j)
              , toVx3 $ f h n (u_!!i) (v_!!(j+1))
              , toVx3 $ f h n (u_!!(i+1)) (v_!!(j+1))
              , toVx3 $ f h n (u_!!(i+1)) (v_!!j)    )
  norm = triangleNormal (x, y, z)
  toVx3 w = Vertex3 (w!!0) (w!!1) (w!!2)

allQuads :: Int -> Int -- numbers of subdivisions for u and v
         -> Double -> Double -- parameters h, n
         -> Map (Int,Int) Quad
allQuads n_u n_v h n =
    M.fromList (zip indices (map (uncurry (quad function h n sequ seqv)) indices))
    where
      indices = [(i,j) | i <- [0 .. n_u-1], j <- [0 .. n_v-1]]
      sequ,seqv :: [Double]
      sequ = [2*pi * frac i n_u | i <- [0 .. n_u]]
      seqv = [2*pi * frac i n_v | i <- [0 .. n_v]]
      frac :: Int -> Int -> Double
      frac p q = realToFrac p / realToFrac q
