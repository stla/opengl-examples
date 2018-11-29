module ConicalSpiral.DataDoublySmooth where
import           Data.Array      (Array, (!), array)
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..))

type Point = (Double, Double, Double)
type Vector = (Double, Double, Double)
type NPoint = (Vertex3 Double, Normal3 Double)
type NTriangle = (NPoint, NPoint, NPoint)

sconical :: Double -> Double -> Double -> Double -> Bool -> Double -> Double
         -> Point
sconical alpha beta gamma n swap u v = (x, e*y, e*z)
  where
    x = alpha * (1-0.5*v/pi) * sin (n*v+0.5*pi) * (1-cos u) +
         gamma * sin (n*v+0.5*pi)
    y = beta*0.5*v/pi + alpha*(1-0.5*v/pi) * sin u
    --y = 0.5*v/pi * (beta - alpha*sin u) + alpha*sin u
    -- z = alpha*(1-0.5*v/pi)*cos(n*v+0.5*pi)*(1-cos u) + gamma*cos(n*v+0.5*pi)
    z = cos(n*v+0.5*pi) * (gamma + alpha*(1-0.5*v/pi)*(1-cos u))
    e = realToFrac $ 1 - 2 * fromEnum swap

cross :: Floating a => (a,a,a) -> (a,a,a) -> (a,a,a)
cross (v1,v2,v3) (w1,w2,w3) =
  (
  v2*w3 - v3*w2,
  v3*w1 - v1*w3,
  v1*w2 - v2*w1
  )

normalize :: Floating a => (a,a,a) -> (a,a,a)
normalize (x,y,z) = (x/n,y/n,z/n)
  where
    n = sqrt(x*x+y*y+z*z)

gradient :: Double -> Double -> Double -> Double -> Bool -> Double -> Double
         -> Vector
gradient alpha beta gamma n swap u v = normalize (cross df_du df_dv)
  where
    dfx_du = alpha*(1-0.5*v/pi) * sin(n*v+0.5*pi) * sin u
    dfy_du = alpha*(1-0.5*v/pi) * cos u
    dfz_du = alpha*(1-0.5*v/pi)*cos(n*v+0.5*pi) * sin u
    df_du = (dfx_du, e*dfy_du, e*dfz_du)
    dfx_dv = n*cos(n*v+0.5*pi) * (gamma + alpha*(1-0.5*v/pi)*(1-cos u)) +
             sin(n*v+0.5*pi) * alpha*0.5/pi*(cos u - 1)
    dfy_dv = beta/2/pi - alpha/2/pi*sin u
    dfz_dv = -n*sin(n*v+0.5*pi) * (gamma + alpha*(1-0.5*v/pi)*(1-cos u)) +
             cos(n*v+0.5*pi) * alpha*0.5/pi*(cos u - 1)
    df_dv = (dfx_dv, e*dfy_dv, e*dfz_dv)
    -- x = alpha * (1-0.5*v/pi) * sin (n*v+0.5*pi) * (1-cos u) +
    --      gamma * sin (n*v+0.5*pi)
    -- x = sin(n*v+0.5*pi) * (gamma + alpha*(1-0.5*v/pi)*(1-cos u))
    -- y = beta*0.5*v/pi + alpha*(1-0.5*v/pi) * sin u
    -- --y = 0.5*v/pi * (beta - alpha*sin u) + alpha*sin u
    -- -- z = alpha*(1-0.5*v/pi)*cos(n*v+0.5*pi)*(1-cos u) + gamma*cos(n*v+0.5*pi)
    -- z = cos(n*v+0.5*pi) * (gamma + alpha*(1-0.5*v/pi)*(1-cos u))
    e = realToFrac $ 1 - 2 * fromEnum swap

frac :: Int -> Int -> Double
frac p q = realToFrac p / realToFrac q

allVertices :: (Double -> Double -> Point) -> [Double] -> [Double]
            -> Array (Int,Int) Point
allVertices f u_ v_ = array ((0,0), (n_u-1,n_v-1)) associations
  where
  n_u = length u_
  n_v = length v_
  indices = [(i,j) | i <- [0 .. n_u-1], j <- [0 .. n_v-1]]
  g (i,j) = ((i,j), f (u_ !! i) (v_ !! j))
  associations = map g indices

allNormals :: Double -> Double -> Double -> Double -> Bool
           -> [Double] -> [Double] -> Array (Int,Int) Vector
allNormals alpha beta gamma n swap u_ v_ =
  array ((0,0), (n_u-1,n_v-1)) associations
  where
  n_u = length u_
  n_v = length v_
  indices = [(i,j) | i <- [0 .. n_u-1], j <- [0 .. n_v-1]]
  g (i,j) = ((i,j), gradient alpha beta gamma n swap (u_ !! i) (v_ !! j))
  associations = map g indices

pointToVertex3 :: Point -> Vertex3 Double
pointToVertex3 (x,y,z) = Vertex3 x y z

vectorToNormal3 :: Vector -> Normal3 Double
vectorToNormal3 (x,y,z) = Normal3 x y z

triangles_ij :: Array (Int,Int) Point -> Array (Int,Int) Vector
            -> Int -> (Int, Int)
            -> (NTriangle, NTriangle)
triangles_ij vertices normals n_u (i,j) =
  (((a,na), (b,nb), (c,nc)), ((c,nc), (b,nb), (d,nd)))
  where
  ip1 = if i==n_u-1 then 0 else i+1
  jp1 = j+1 
  a = pointToVertex3 $ vertices ! (i,j)
  na = vectorToNormal3 $ normals ! (i,j)
  c = pointToVertex3 $ vertices ! (i,jp1)
  nc = vectorToNormal3 $ normals ! (i,jp1)
  d = pointToVertex3 $ vertices ! (ip1,jp1)
  nd = vectorToNormal3 $ normals ! (ip1,jp1)
  b = pointToVertex3 $ vertices ! (ip1,j)
  nb = vectorToNormal3 $ normals ! (ip1,j)

allTriangles :: Double -> Double -> Double -> Double -> Bool -> (Int,Int)
             -> [(NTriangle,NTriangle)]
allTriangles alpha beta gamma n swap (n_u,n_v) =
  map (triangles_ij vertices normals n_u) indices
  where
  u_ = [2*pi * frac i n_u | i <- [0 .. n_u-1]]
  v_ = [2*pi * frac i n_v | i <- [0 .. n_v]]
  vertices = allVertices (sconical alpha beta gamma n swap) u_ v_
  normals = allNormals alpha beta gamma n swap u_ v_
  indices = [(i,j) | i <- [0 .. n_u-1], j <- [0 .. n_v-1]]

allTriangles' :: Double -> Double -> Double -> Double -> (Int,Int)
             -> [(NTriangle,NTriangle)]
allTriangles' alpha beta gamma n (n_u,n_v) = 
  (allTriangles alpha beta gamma n False (n_u,n_v)) ++
    (allTriangles alpha beta gamma n True (n_u,n_v))
