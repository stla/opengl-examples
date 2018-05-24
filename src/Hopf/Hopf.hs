module Hopf.Hopf where
import           Graphics.Rendering.OpenGL.GL (Vertex3 (..), GLfloat)

hopf :: [GLfloat] -> [GLfloat]
hopf p = [ p!!0*p!!0 + p!!1*p!!1 - p!!2*p!!2 - p!!3*p!!3
         , 2 * (p!!1*p!!3 - p!!0*p!!2)
         , 2 * (p!!0*p!!3 + p!!1*p!!2) ]

hopf' :: [GLfloat] -> Vertex3 GLfloat
hopf' = toVx3 . hopf
  where
    toVx3 x = Vertex3 (x!!0) (x!!1) (x!!2)

hopfinverse :: [GLfloat] -> GLfloat -> [GLfloat]
hopfinverse q t =
  map ((*) (1 / sqrt (2 * (1+(q!!2)))))
      [cos t * (q!!0) + sin t * (q!!1),
       sin t * (1+(q!!2)),
       cos t * (1+(q!!2)),
       sin t * (q!!0) - cos t * (q!!1)]


hopfinverse' :: [GLfloat] -> GLfloat -> Vertex3 GLfloat
hopfinverse' q t = toVx3 (hopfinverse q t)
   where
     toVx3 x = Vertex3 (x!!0) (x!!1) (x!!2)


stereoProj :: [GLfloat] -> [GLfloat]
stereoProj x = map (/(1-x!!3)) [2 * x!!0, 2 * x!!1, 2 * x!!2]
