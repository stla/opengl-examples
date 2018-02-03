module Utils.OpenGL
  where
import Graphics.Rendering.OpenGL.GL

vertex3f :: Vertex3 GLfloat -> IO ()
vertex3f = vertex

triangleNormal :: Floating a => (Vertex3 a, Vertex3 a, Vertex3 a) -> Normal3 a
triangleNormal (Vertex3 x1 x2 x3, Vertex3 y1 y2 y3, Vertex3 z1 z2 z3) =
  Normal3 a' b' c'
  where
    (a, b, c) = crossProd (y1-x1, y2-x2, y3-x3) (z1-x1, z2-x2, z3-x3)
    crossProd (a1,a2,a3) (b1,b2,b3) = (a2*b3-a3*b2, a3*b1-a1*b3, a1*b2-a2*b1)
    norm = sqrt (a*a + b*b + c*c)
    a' = a / norm
    b' = b / norm
    c' = c / norm
