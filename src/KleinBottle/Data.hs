module KleinBottle.Data where
import           Graphics.Rendering.OpenGL.GL (GLdouble, Normal3 (..),
                                               Vertex3 (..))
import           Utils.OpenGL                 (triangleNormal)

pow :: (Double, Int) -> Double
pow (x,n) = product (replicate n x)


cX :: Double -> Double -> Double
cX uu vv =
  0.75*(1-cos(uu)+1/3*(pow(2-uu/pi,3)-pow(2-uu/pi,2)*11/4+
                        3/2*(2-uu/pi)+0.8)*cos(vv)*(-cos(uu)/2+3/4*pow(sin(uu),2)*cos(uu)+
                                                     cos(2*uu)/2)*1/sqrt(pow(sin(uu),2)+pow(-cos(uu)/2+3/4*pow(sin(uu),2)*cos(uu)+cos(2*uu)/2,2)))

cY :: Double -> Double -> Double
cY uu vv =
  1/3*(pow(2-uu/pi,3)-pow(2-uu/pi,2)*11/4+3/2*(2-uu/pi)+0.8)*sin(vv)


cZ :: Double -> Double -> Double
cZ uu vv =
  pow(1-cos(uu),2)*sin(uu)/4+1/3*(pow(2-uu/pi,3)-pow(2-uu/pi,2)*11/4+3/2*(2-uu/pi)+0.8)*cos(vv)*sin(uu)*
    1/sqrt(pow(sin(uu),2)+pow(-cos(uu)/2+3/4*pow(sin(uu),2)*cos(uu)+cos(2*uu)/2,2))

kleinFun :: Double -> Double -> [Double]
kleinFun uu vv = [cX uu vv, cY uu vv, cZ uu vv]

kleinFun' :: Double -> Double -> Vertex3 Double
kleinFun' uu vv = toVx3 $ kleinFun uu vv
  where
  toVx3 x = Vertex3 (x!!0) (x!!1) (x!!2)

quad :: (Double -> Double -> Vertex3 Double) -> [Double] -> [Double] -> Int -> Int
     -> ((Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double),
          Normal3 Double)
quad f u_ v_ i j = ((a, b, c, d), norm)
  where
  (a,b,c,d) = ( f (u_!!i) (v_!!j)
              , f (u_!!i) (v_!!(j+1))
              , f (u_!!(i+1)) (v_!!(j+1))
              , f (u_!!(i+1)) (v_!!j) )
  norm = triangleNormal (a, b, c)

allQuads :: Int
         -> [((Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double),
               Normal3 Double)]
allQuads n = map (uncurry (quad kleinFun' sequ seqv))
                  [(i,j) | i <- [0 .. length sequ - 2], j <- [0 .. length seqv - 2]]
        where
            sequ,seqv :: [Double]
            sequ = [2 * pi * frac i n | i <- [1 .. n-1]]
            seqv = [2 * pi * frac i n | i <- [0 .. n]]
            frac :: Int -> Int -> Double
            frac p q = realToFrac p / realToFrac q
