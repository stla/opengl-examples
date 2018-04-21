module Apple.Data
  (allQuads, Quad)
  where
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as M
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..))
import           Utils.OpenGL                 (triangleNormal)

type Quad = ((Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double), Normal3 Double)

function :: Double -> Double -- parameters r1, r2
         -> Double -> Double -- variables u and v
         -> [Double]         -- result vector
function r1 r2 u v = [x, y, z]
  where
    x = cos u * (r1 + r2*cos v) + (v/pi)^100
    y = sin u * (r1 + r2*cos v) + 0.25*cos(5*u)
    z = -2.3*log(1-0.3157*v) + 6*sin v + 2*cos v

quad :: (Double -> Double -> Double -> Double -> [Double]) -- the function
     -> Double -> Double               -- parameters r1, r2
     -> [Double] -> [Double]                     -- sequences of u and v
     -> Int -> Int                               -- indices
     -> Quad
quad f r1 r2 u_ v_ i j = ((x, y, z, t), norm)
  where
  (x,y,z,t) = ( toVx3 $ f r1 r2 (u_!!i) (v_!!j)
              , toVx3 $ f r1 r2 (u_!!i) (v_!!(j+1))
              , toVx3 $ f r1 r2 (u_!!(i+1)) (v_!!(j+1))
              , toVx3 $ f r1 r2 (u_!!(i+1)) (v_!!j)    )
  norm = triangleNormal (x, y, z)
  toVx3 w = Vertex3 (w!!0) (w!!1) (w!!2)

allQuads :: Int -> Int -- numbers of subdivisions for u and v
         -> Double -> Double  -- parameters r1, r2
         -> Map (Int,Int) Quad
allQuads n_u n_v r1 r2 =
    M.fromList (zip indices (map (uncurry (quad function r1 r2 sequ seqv)) indices))
    where
      indices = [(i,j) | i <- [0 .. n_u-1], j <- [0 .. n_v-1]]
      sequ,seqv :: [Double]
      sequ = [2*pi * frac i n_u | i <- [0 .. n_u]]
      seqv = [-pi + 2*pi * frac i n_v | i <- [0 .. n_v]]
      frac :: Int -> Int -> Double
      frac p q = realToFrac p / realToFrac q
