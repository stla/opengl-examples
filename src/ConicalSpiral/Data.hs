module ConicalSpiral.Data where
import           Data.List
import           Graphics.Rendering.OpenGL.GL (GLdouble, Normal3 (..),
                                               Vertex3 (..))
import           Utils.OpenGL                 (triangleNormal)

sconical :: Double -> Double -> Double -> Double -> Double -> Double -> [Double]
sconical alpha beta gamma n u v = [x, y, z]
  where
    x = alpha * (1-0.5*v/pi) * sin (n*v+0.5*pi) * (1-cos u) +
         gamma * sin (n*v+0.5*pi)
    y = beta*0.5*v/pi + alpha*(1-0.5*v/pi) * sin u
    z = alpha*(1-0.5*v/pi)*cos(n*v+0.5*pi)*(1-cos u) + gamma*cos(n*v+0.5*pi)

sconical' :: Double -> Double -> Double -> Double -> Double -> Double
          -> Vertex3 Double
sconical' alpha beta gamma n u v = toVx3 $ sconical alpha beta gamma n u v
  where
    toVx3 x = Vertex3 (x!!0) (x!!1) (x!!2)

quad :: (Double -> Double -> Double -> Double -> Double -> Double -> Vertex3 Double)
     -> Double -> Double -> Double -> Double
     -> [Double] -> [Double] -> Int -> Int
     -> ((Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double),
          Normal3 Double)
quad f alpha beta gamma n u_ v_ i j = ((a, b, c, d), norm)
  where
  (a,b,c,d) = ( f alpha beta gamma n (u_!!i) (v_!!j)
              , f alpha beta gamma n (u_!!i) (v_!!(j+1))
              , f alpha beta gamma n (u_!!(i+1)) (v_!!(j+1))
              , f alpha beta gamma n (u_!!(i+1)) (v_!!j) )
  norm = triangleNormal (a, b, c)

allQuads :: Int -> Double -> Double -> Double -> Double
         -> [((Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double),
               Normal3 Double)]
allQuads m alpha beta gamma n =
    map (uncurry (quad sconical' alpha beta gamma n sequ seqv))
        [(i,j) | i <- [0 .. length sequ - 2], j <- [0 .. length seqv - 2]]
        where
            sequ,seqv :: [Double]
            sequ = [2*pi * frac i m | i <- [0 .. m]]
            seqv = [2*pi * frac i m | i <- [0 .. m]]
            frac :: Int -> Int -> Double
            frac p q = realToFrac p / realToFrac q

