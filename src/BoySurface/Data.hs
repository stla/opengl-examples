module BoySurface.Data where
import           Data.Complex
import           Graphics.Rendering.OpenGL.GL (GLdouble, Normal3 (..),
                                               Vertex3 (..))
import           Utils.OpenGL                 (triangleNormal)


boyFun :: Double -> Double -> [Double]
boyFun a b =
    map (/ xyzNorm2) xyz
        where
        w = a :+ b
        u = w^^6 + sqrt 5 * w^^3 - 1
        z = w * (1 - w^^4) / u
        g1 = -1.5 * imagPart z
        g2 = -1.5 * realPart z
        g3 = imagPart ( (1+w^^6) / u ) - 0.5
        xyz = [g1, g2, g3]
        xyzNorm2 = sum $ zipWith (*) xyz xyz

boyFun' :: Double -> Double -> Vertex3 Double
boyFun' a b = toVx3 $ boyFun a b
  where
    toVx3 x = Vertex3 (x!!0) (x!!1) (x!!2)

quad :: (Double -> Double -> Vertex3 Double) -> [Double] -> [Double] -> Int -> Int
     -> ((Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double), Normal3 Double)
quad f u_ v_ i j = ((a, b, c, d), norm)
  where
  (a,b,c,d) = ( f (u_!!i) (v_!!j)
              , f (u_!!i) (v_!!(j+1))
              , f (u_!!(i+1)) (v_!!(j+1))
              , f (u_!!(i+1)) (v_!!j) )
  norm = triangleNormal (a, b, c)

allQuads :: Int
         -> [((Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double), Normal3 Double)]
allQuads n = map (uncurry (quad boyFun' sequ seqv))
                 [(i,j) | i <- [0 .. length sequ - 2], j <- [0 .. length seqv - 2]]
        where
            sequ,seqv :: [Double]
            sequ = [cos $ frac i n * 2 * pi | i <- [0 .. n]]
            seqv = [sin $ frac i n * 2 * pi | i <- [0 .. n]]
            frac :: Int -> Int -> Double
            frac p q = realToFrac p / realToFrac q

