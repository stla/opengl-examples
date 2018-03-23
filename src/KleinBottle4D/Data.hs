module KleinBottle4D.Data where
import           Graphics.Rendering.OpenGL.GL (GLdouble, Normal3 (..),
                                               Vertex3 (..))
import           Tesseract.Transformations4D
import           Utils.OpenGL                 (triangleNormal)

klein4 :: Double -> Double -> [[Double]]
klein4 a b = map (\(u,v) ->
    [ (a + b * cos v) * cos u
    , (a + b * cos v) * sin u
    , b * sin v * cos (u/2)
    , b * sin v * sin (u/2) ]) [(u',v') | u' <- u_, v' <- v_]
  where
    u_ = [frac i 100 * 2 * pi | i <- [0 .. 100]]
    v_ = [frac i 100 * 2 * pi | i <- [0 .. 100]]
    frac :: Int -> Int -> Double
    frac p q = realToFrac p / realToFrac q

klein4rotated :: Double -> Double -> Double -> [[Double]]
klein4rotated a b alpha =
    map (rotate4D 0 0 alpha) (klein4 a b)

stereoproj :: [Double] -> [Double]
stereoproj x = map (/(r - last x)) (map (*(2*r)) (init x))
  where
  r = sqrt $ sum (zipWith (*) x x)

klein4rotatedAndProjected :: Double -> Double -> Double -> [[Double]]
klein4rotatedAndProjected a b alpha =
    map stereoproj (klein4rotated a b alpha)

-- un [Double] correspond à l'image d'un couple (u,v)
-- alors faire rotate et stereo aux quads
fklein4 :: Double -> Double -> Double -> Double -> [Double]
fklein4 a b u v =
    [ a * (cos (u/2) * cos v - sin (u/2) * sin (2*v))
    , a * (sin (u/2) * cos v + cos (u/2) * sin (2*v))
    , b * cos u * (1 + 0.5 * sin v)
    , b * sin u * (1 + 0.5 * sin v) ]
--    [ (a + b * cos v) * cos u
--    , (a + b * cos v) * sin u
--    , b * sin v * cos (u/2)
--    , b * sin v * sin (u/2) ]


fklein4' :: Double -> Double -> Double -> Double -> Vertex3 Double
fklein4' a b u v = toVx3 $ fklein4 a b u v
  where
  toVx3 x = Vertex3 (x!!0) (x!!1) (x!!2)

quad4D :: (Double -> Double -> Double -> Double -> [Double])
       -> [Double] -> [Double] -> Int -> Int -> Double -> Double
       -> [[Double]]
quad4D f u_ v_ i j a b = [x, y, z, t]
  where
    (x, y, z, t) = (  f a b (u_!!i) (v_!!j)
                    , f a b (u_!!i) (v_!!(j+1))
                    , f a b (u_!!(i+1)) (v_!!(j+1))
                    , f a b (u_!!(i+1)) (v_!!j) )

quad3D :: [Double] -> [Double] -> Int -> Int -> Double -> Double -> Double
       -> [[Double]]
quad3D u_ v_ i j a b alpha = [x'', y'', z'', t'']
  where
    [x,y,z,t] = quad4D fklein4 u_ v_ i j a b
    [x',y',z',t'] = map (rotation4Dplane [1,0,0,0] [0,0,0,1] alpha) [x,y,z,t]
--    [x'',y'',z'',t''] = map stereoproj [x',y',z',t']
    [x'',y'',z'',t''] = map init [x',y',z',t']

quad :: [Double] -> [Double] -> Int -> Int -> Double -> Double -> Double
     -> ((Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double),
          Normal3 Double)
quad u_ v_ i j a b alpha = ((x', y', z', t'), norm)
  where
  [x,y,z,t] = quad3D u_ v_ i j a b alpha
  x' = toVx3 x
  y' = toVx3 y
  z' = toVx3 z
  t' = toVx3 t
  toVx3 v = Vertex3 (v!!0) (v!!1) (v!!2)
  norm = triangleNormal (x', y', z')

allQuads :: Double -> Double -> Double
         -> [((Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double),
               Normal3 Double)]
allQuads a b alpha =
    map (\(i,j) -> quad sequ seqv i j a b alpha)
    [(i,j) | i <- [0 .. length sequ - 2], j <- [0 .. length seqv - 2]]
        where
            n = 50
            sequ,seqv :: [Double]
            sequ = [frac i n * 2 * pi | i <- [0 .. n]]
            seqv = [frac i n * 2 * pi | i <- [0 .. n]]
            frac :: Int -> Int -> Double
            frac p q = realToFrac p / realToFrac q
