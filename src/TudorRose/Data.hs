module TudorRose.Data
  (allQuads, Quad)
  where
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..))
import           Utils.OpenGL                 (triangleNormal)

type Quad = ((Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double), Normal3 Double)

function1 :: Double -> Double -> [Double]
function1 u v = [x, y, z]
  where
    r = cos v * cos v * abs(sin(4*u))
    x = r * cos u * cos v + 2
    y = r * sin u * cos v
    z = r * sin v

function2 :: Double -> Double -> [Double]
function2 u v = [x, y, z]
  where
    r = cos v * cos v * max (abs(sin(4*u))) (0.9-0.2*abs(cos(8*u)))
    x = r * cos u * cos v
    y = r * sin u * cos v
    z = r * sin v * 0.5

function3 :: Double -> Double -> [Double]
function3 u v = [x, y, z]
  where
    r = cos v * cos v * (0.9-0.2*abs(cos(8*u)))
    x = r * cos u * cos v + 1
    y = r * sin u * cos v + 2
    z = r * sin v

quad :: (Double -> Double -> [Double]) -- the function
     -> [Double] -> [Double]           -- sequences of u and v
     -> Int -> Int                     -- indices
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
    map (uncurry (quad function1 sequ seqv)) indices ++
    map (uncurry (quad function2 sequ seqv)) indices ++
    map (uncurry (quad function3 sequ seqv)) indices
    where
      indices = [(i,j) | i <- [0 .. n_u-1], j <- [0 .. n_v-1]]
      sequ,seqv :: [Double]
      sequ = [2*pi * frac i n_u | i <- [0 .. n_u]]
      seqv = [2*pi * frac i n_v | i <- [0 .. n_v]]
      frac :: Int -> Int -> Double
      frac p q = realToFrac p / realToFrac q
