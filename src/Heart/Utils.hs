module Heart.Utils
  (triangleColor)
  where
import           Graphics.Rendering.OpenGL.GL (Color4 (..), GLfloat,
                                               Vertex3 (..))

triangleCenter :: Floating a => (Vertex3 a, Vertex3 a, Vertex3 a) -> (a, a, a)
triangleCenter triangle = (x/3 , y/3, z/3)
  where
    (a, b, c) = triangle
    Vertex3 a1 a2 a3 = a
    Vertex3 b1 b2 b3 = b
    Vertex3 c1 c2 c3 = c
    x = a1 + b1 + c1
    y = a2 + b2 + c2
    z = a3 + b3 + c3

black,red :: Color4 GLfloat
black      = Color4    0    0    0    1
red        = Color4    1    0    0    1

triangleColor :: (Real a, Floating a) => (Vertex3 a, Vertex3 a, Vertex3 a)
              -> Color4 GLfloat
triangleColor triangle =
  if z<0 then red else black
  where
    (_,_,z) = triangleCenter triangle
