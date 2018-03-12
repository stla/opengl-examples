module Hopf.Hopf where
import           Graphics.Rendering.OpenGL.GL (GLdouble, Vertex3 (..))

hopf :: [Double] -> [Double]
hopf p = [ p!!0*p!!0 + p!!1*p!!1 - p!!2*p!!2 - p!!3*p!!3
         , 2 * (p!!1*p!!3 - p!!0*p!!2)
         , 2 * (p!!0*p!!3 + p!!1*p!!2) ]

hopf' :: [Double] -> Vertex3 Double
hopf' = toVx3 . hopf
  where
    toVx3 x = Vertex3 (x!!0) (x!!1) (x!!2)

hopfinverse :: [Double] -> Double -> [Double]
hopfinverse q t =
  map ((*) (1 / sqrt (2 * (1+(q!!0)))))
      [-sin t * (1+(q!!0))
      , cos t * (1+(q!!0))
      -- , cos t * (q!!1) - sin t * (q!!2)
      -- , cos t * (q!!2) + sin t * (q!!1) ]
      , cos t * (q!!1) + sin t * (q!!2)
      , cos t * (q!!2) - sin t * (q!!1)]


-- # hopfinverse' :: [Double] -> Double -> Vertex3 Double
-- # hopfinverse' = toVx3 . hopfinverse
-- #   where
-- #     toVx3 x
