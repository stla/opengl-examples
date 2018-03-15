module BianchiPinkall.Helpers where

import           Graphics.Rendering.OpenGL.GL (GLdouble, Vertex3 (..))


flatTorus :: Int -> Double -> Double -> [Double]
flatTorus n u v = [ cos a * cos (u+v)
                  , cos a * sin (u+v)
                  , sin a * cos (u-v)
                  , sin a * sin (u-v) ]
  where
  a = 0.5 + 0.5 * sin (realToFrac n * 2 * v)

-- modified stereographic projection
stereom :: [Double] -> [Double]
stereom p = [r * p!!0, r * p!!1, r * p!!2]
  where
    r = acos(p!!3) / pi / sqrt (1 - (p!!3)*(p!!3))


gridUV :: [(Double, Double)]
gridUV = [(u,v) | u <- sequ, v <- seqv]
  where
    sequ = [realToFrac i/100 * 2 * pi | i <- [0 .. 99]]
    seqv = [realToFrac i/100 * pi | i <- [0 .. 99]]

allCirclesS3 :: Int -> [[Double]]
allCirclesS3 n = map (\(u,v) -> flatTorus n u v) gridUV -- uncurry flatorus n

stereoCircles :: Int -> [[Double]]
stereoCircles n = map stereom (allCirclesS3 n)

stereoCircles' :: Int -> [Vertex3 Double]
stereoCircles' n = map toVx3 (stereoCircles n)
  where
    toVx3 x = Vertex3 (x!!0) (x!!1) (x!!2)

stereoCirclesDupli :: Int -> [[[Double]]] -- https://stackoverflow.com/questions/49298586/expanding-a-list-to-a-path/49298820#49298820
stereoCirclesDupli n = map (\i -> [input!!i, input!!(i+1)]) [0 .. length input-2] -- zipWith (\a b -> [a,b]) xs (tail xs)
  where
  input = stereoCircles n

stereoCirclesDupli' :: Int -> [[Vertex3 Double]]
stereoCirclesDupli' n = map (map toVx3) (stereoCirclesDupli n)
  where
  toVx3 x = Vertex3 (x!!0) (x!!1) (x!!2)
