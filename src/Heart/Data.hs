module Heart.Data
  (allTriangles, NTriangle)
  where
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..))
import           Utils.OpenGL                 (triangleNormal)

type NTriangle = ((Vertex3 Double, Vertex3 Double, Vertex3 Double), Normal3 Double)

fun :: Double -> Double -> [Double]
fun theta z = [x, y, z]
  where
    r = 4 * sqrt(1 - z*z) * sin(abs theta) ** abs theta
    x = r * sin theta
    y = r * cos theta

fun' :: Double -> Double -> Vertex3 Double
fun' theta z = toVx3 $ fun theta z
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

allTriangles :: Int -> [NTriangle]
allTriangles m  =
    concatMap (uncurry (triangle fun' sequ seqv))
        [(i,j) | i <- [0 .. length sequ - 2], j <- [0 .. length seqv - 2]]
        where
            sequ,seqv :: [Double]
            sequ = [-pi + 2*pi * frac i m | i <- [0 .. m]]
            seqv = [-0.98 + 1.96 * frac i m | i <- [0 .. m]]
            frac :: Int -> Int -> Double
            frac p q = realToFrac p / realToFrac q
