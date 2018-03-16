module BianchiPinkall.Helpers where

-- import           Data.List.Index
import           Graphics.Rendering.OpenGL.GL (GLdouble, Normal3 (..),
                                               Vertex3 (..))
import           Utils.OpenGL                 (triangleNormal)

flatTorus :: Double -> Double -> Double -> [Double]
flatTorus n u v = [ cos a * cos (u+v)
                  , cos a * sin (u+v)
                  , sin a * cos (u-v)
                  , sin a * sin (u-v) ]
  where
  a = 0.5 + 0.5 * sin (n * 2 * v)

-- modified stereographic projection
stereom :: [Double] -> [Double]
stereom p = [r * p!!0, r * p!!1, r * p!!2]
  where
    r = acos(p!!3) / pi / sqrt (1 - (p!!3)*(p!!3))

pinkallFun :: Double -> Double -> Double -> [Double]
pinkallFun n u v = stereom (flatTorus n u v)

pinkallFun' :: Double -> Double -> Double -> Vertex3 Double
pinkallFun' n u v = toVx3 $ pinkallFun n u v
  where
  toVx3 x = Vertex3 (x!!0) (x!!1) (x!!2)

sequ,seqv :: [Double]
sequ = [realToFrac i/200 * 2 * pi | i <- [0 .. 200]]
seqv = [realToFrac i/200 * pi | i <- [0 .. 200]]

quad :: Double -> [Double] -> [Double] -> Int -> Int
     -> ((Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double), Normal3 Double)
quad n u_ v_ i j = ((a, b, c, d), norm)
  where
  (a,b,c,d) = ( pinkallFun' n (u_!!i) (v_!!j)
              , pinkallFun' n (u_!!i) (v_!!(j+1))
              , pinkallFun' n (u_!!(i+1)) (v_!!(j+1))
              , pinkallFun' n (u_!!(i+1)) (v_!!j) )
  norm = triangleNormal (a, b, c)

allQuads :: Double -> [((Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double), Normal3 Double)]
allQuads n = map (uncurry (quad n sequ seqv)) [(i,j) | i <- [0 .. length sequ - 2], j <- [0 .. length seqv - 2]]
