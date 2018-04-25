module Hopf.HopfCircle
  (vCircle)
  where
import           Graphics.Rendering.OpenGL.GL (Vertex3 (..))

scircHinv :: Double -> Double -> Double -> Vertex3 Double
scircHinv theta phi xi = Vertex3 x y z
  where
  x' = cos(theta/2)*cos(xi+phi/2)
  y' = cos(theta/2)*sin(xi+phi/2)
  z' = sin(theta/2)*cos(xi-phi/2)
  [x,y,z] = map (/ (1-sin(theta/2)*sin(xi-phi/2))) [x',y',z']

vCircle :: Double -> Double -> [Vertex3 Double]
vCircle theta phi = map (scircHinv theta phi) xi
  where
  xi = [2*pi * realToFrac i / realToFrac 200 | i <- [0..200]]
