module ConicalSpiral.DataDoubly where
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..))
import           Utils.OpenGL                 (triangleNormal)

sconical :: Double -> Double -> Double -> Double -> Bool
         -> Double -> Double
         -> [Double]
sconical alpha beta gamma n swap u v = [x, e*y, e*z]
  where
    x = alpha * (1-0.5*v/pi) * sin (n*v+0.5*pi) * (1-cos u) +
         gamma * sin (n*v+0.5*pi)
    y = beta*0.5*v/pi + alpha*(1-0.5*v/pi) * sin u
    z = alpha*(1-0.5*v/pi)*cos(n*v+0.5*pi)*(1-cos u) + gamma*cos(n*v+0.5*pi)
    e = realToFrac $ 1 - 2 * fromEnum swap

quad :: (Double -> Double -> Double -> Double -> Bool -> Double -> Double -> [Double])
     -> Double -> Double -> Double -> Double -> Bool
     -> [Double] -> [Double] -> Int -> Int
     -> ((Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double),
          Normal3 Double)
quad f alpha beta gamma n swap u_ v_ i j = ((a, b, c, d), norm)
  where
  (a,b,c,d) = ( toVx3 $ f alpha beta gamma n swap (u_!!i) (v_!!j)
              , toVx3 $ f alpha beta gamma n swap (u_!!i) (v_!!(j+1))
              , toVx3 $ f alpha beta gamma n swap (u_!!(i+1)) (v_!!(j+1))
              , toVx3 $ f alpha beta gamma n swap (u_!!(i+1)) (v_!!j) )
  norm = triangleNormal (a, b, c)
  toVx3 x = Vertex3 (x!!0) (x!!1) (x!!2)

allQuads :: Int
         -> Double -> Double -> Double -> Double
         -> [((Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double),
               Normal3 Double)]
allQuads m alpha beta gamma n =
    map (uncurry (quad sconical alpha beta gamma n False sequ seqv)) ijs ++
    map (uncurry (quad sconical alpha beta gamma n True sequ seqv)) ijs
        where
            sequ,seqv :: [Double]
            sequ = [2*pi * frac i m | i <- [0 .. m]]
            seqv = [2*pi * frac i m | i <- [0 .. m]]
            frac :: Int -> Int -> Double
            frac p q = realToFrac p / realToFrac q
            ijs = [(i,j) | i <- [0 .. length sequ - 2], j <- [0 .. length seqv - 2]]
