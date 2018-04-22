module Euphonium.Data
  (allQuads, Quad)
  where
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..))
import           Utils.OpenGL                 (triangleNormal)

type Quad = ((Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double), Normal3 Double)

flare :: Double -> Double
flare u = u/(2*pi) + 2*(u/(2*pi))^20

function :: Double -> Double -- variables u and v
         -> [Double]         -- result vector
function u v = [x, y, z]
  where
    x = (2 + u/(2*pi) * cos v) * sin u - 0.1*u
    y = (2 + flare u * cos v) * cos u + 0.5*u
    z = flare u * sin v

quad :: (Double -> Double -> [Double]) -- the function
     -> [Double] -> [Double]                     -- sequences of u and v
     -> Int -> Int                               -- indices
     -> Quad
quad f u_ v_ i j = ((x, y, z, t), norm)
  where
  (x,y,z,t) = ( toVx3 $ f (u_!!i) (v_!!j)
              , toVx3 $ f (u_!!i) (v_!!(j+1))
              , toVx3 $ f (u_!!(i+1)) (v_!!(j+1))
              , toVx3 $ f (u_!!(i+1)) (v_!!j)    )
  norm = triangleNormal (x, y, z)
  toVx3 w = Vertex3 (w!!0) (w!!1) (w!!2)

allQuads :: Int -> Int -- numbers of subdivisions for u and v
         -> [Quad]
allQuads n_u n_v =
    map (uncurry (quad function sequ seqv)) indices
    where
      indices = [(i,j) | i <- [0 .. n_u-1], j <- [0 .. n_v-1]]
      sequ,seqv :: [Double]
      sequ = [2*pi * frac i n_u | i <- [0 .. n_u]]
      seqv = [2*pi * frac i n_v | i <- [0 .. n_v]]
      frac :: Int -> Int -> Double
      frac p q = realToFrac p / realToFrac q
