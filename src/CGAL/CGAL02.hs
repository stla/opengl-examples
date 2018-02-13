module CGAL.CGAL02
  (polygons, vertices)
  where
import           Graphics.Rendering.OpenGL.GL (GLfloat, Vertex3 (..))
import           System.IO.Unsafe             (unsafePerformIO)
import           Utils.ReadOFF

{-# NOINLINE verticesAndPolygonsIdx #-}
verticesAndPolygonsIdx :: ([Vertex3 GLfloat], [[Int]])
verticesAndPolygonsIdx = (map toVertex3 v, f)
  where
    (v, f) = unsafePerformIO $ readOFF "off/CGAL02.off"
    toVertex3 flts = Vertex3 (flts!!0) (flts!!1) (flts!!2)

polygons :: [[Vertex3 GLfloat]]
polygons = map (\idxs -> [vertices!!i | i <- idxs]) polygonsIdxs
  where
    (vertices, polygonsIdxs) = verticesAndPolygonsIdx

vertices :: [Vertex3 GLfloat]
vertices = fst verticesAndPolygonsIdx
