module Utils.Quads.Color where
import           Graphics.Rendering.OpenGL.GL (Color4 (..), GLfloat,
                                               Vertex3 (..))

type Quad = (Vertex3 Double, Vertex3 Double, Vertex3 Double, Vertex3 Double)

colMeans :: Quad -> (Double, Double, Double)
colMeans quad = (x/m , y/m, z/m)
  where
    (a, b, c, d) = quad
    Vertex3 a1 a2 a3 = a
    Vertex3 b1 b2 b3 = b
    Vertex3 c1 c2 c3 = c
    Vertex3 d1 d2 d3 = d
    x = abs (a1 + b1 + c1 + d1)
    y = abs (a2 + b2 + c2 + d2)
    z = abs (a3 + b3 + c3 + d3)
    m = x + y + z

quadColor :: Quad -> Color4 GLfloat
quadColor quad = Color4 (realToFrac x) (realToFrac y) (realToFrac z) 1
  where
    (x,y,z) = colMeans quad
