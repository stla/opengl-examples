module WavyCylinder.Data where
-- import           Data.List
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..))
import           Utils.OpenGL                 (triangleNormal)

function :: Float -> Float -> Float -> Float -> Float -> Float -> [Float]
function r alpha beta n u v = [x, y, z]
  where
    x = (r + alpha * sin(n*u)) * cos(u + z**beta - 1)
    y = (r + alpha * sin(n*u)) * sin(u + z**beta - 1)
    z = v


quad :: (Float -> Float -> Float -> Float -> Float -> Float -> [Float])
     -> Float -> Float -> Float -> Float
     -> [Float] -> [Float]
     -> Int -> Int
     -> ((Vertex3 Float, Vertex3 Float, Vertex3 Float, Vertex3 Float),
          Normal3 Float)
quad f r alpha beta n u_ v_ i j = ((a, b, c, d), norm)
  where
  (a,b,c,d) = ( toVx3 $ f r alpha beta n (u_!!i) (v_!!j)
              , toVx3 $ f r alpha beta n (u_!!i) (v_!!(j+1))
              , toVx3 $ f r alpha beta n (u_!!(i+1)) (v_!!(j+1))
              , toVx3 $ f r alpha beta n (u_!!(i+1)) (v_!!j) )
  norm = triangleNormal (a, b, c)
  toVx3 x = Vertex3 (x!!0) (x!!1) (x!!2)

allQuads :: Int
         -> Float -> Float -> Float -> Float
         -> [((Vertex3 Float, Vertex3 Float, Vertex3 Float, Vertex3 Float),
               Normal3 Float)]
allQuads m r alpha beta n =
    map (uncurry (quad function r alpha beta n sequ seqv))
        [(i,j) | i <- [0 .. m-1], j <- [0 .. m-1]]
        where
            sequ,seqv :: [Float]
            sequ = [2*pi * frac i m | i <- [0 .. m]]
            seqv = [1 + 20 * frac i m | i <- [0 .. m]]
            frac :: Int -> Int -> Float
            frac p q = realToFrac p / realToFrac q

