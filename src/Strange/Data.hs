module Strange.Data
  (strange)
  where
import           Control.Arrow                (first)
import           Graphics.Rendering.OpenGL.GL
import           Utils.Colour                 (interpolateColor)
import           Utils.OpenGL                 (triangleNormal)

type Vx = Vertex3 GLfloat
type Triangle = (Vx, Vx, Vx)
type CVertex = (Vx, Color4 GLfloat)
type CTriangle = (CVertex, CVertex, CVertex)

vertices :: [Vx]
vertices =
  map (\x -> Vertex3 (sin (pi*x) * cos (2*pi*x))
                     (sin (pi*x) * sin (2*pi*x))
                     (cos (pi*x)))
      [i/200 | i <- [1 .. 199]]

north,south :: Vx
north = Vertex3 0 0 1
south = Vertex3 0 0 (-1)

toTriangle :: Vx -> Vx -> Vx -> (Triangle, Normal3 GLfloat)
toTriangle v0 v1 v2 = ((v0,v1,v2), triangleNormal (v0,v1,v2))

triangles :: [(Triangle, Normal3 GLfloat)]
triangles = triangles1 ++ triangles2
  where
  triangles1 = zipWith (toTriangle north) (init vertices) (tail vertices)
  triangles2 = zipWith (toTriangle south) (tail vertices) (init vertices)

colorizeVertex :: Vx -> CVertex
colorizeVertex v = (v, interpolateColor (0,0,1) (1,1,0) (1,0,0) ((z+1)/2))
  where
    Vertex3 _ _ z = v

colorizeTriangle :: Triangle -> CTriangle
colorizeTriangle (v1,v2,v3) =
  (colorizeVertex v1, colorizeVertex v2, colorizeVertex v3)

strange :: [(CTriangle, Normal3 GLfloat)]
strange = map (first colorizeTriangle) triangles
