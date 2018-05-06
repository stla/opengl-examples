module Pinecone.Data
  where
import           Graphics.Rendering.OpenGL.GL (GLfloat, Vertex3 (..))
import           System.IO.Unsafe
import           Utils.ReadOBJ

verticesAndFaces :: ([[Float]] , [[Int]])
{-# NOINLINE verticesAndFaces #-}
verticesAndFaces = unsafePerformIO $ readOBJ "OBJ/pinecone.obj"

allVertices :: [[Float]]
allVertices = fst verticesAndFaces

faces :: [[Int]]
faces = map (map (subtract 1)) (snd verticesAndFaces)

polygons :: [[Vertex3 GLfloat]]
polygons = map (\idxs -> [toVx3 $ allVertices !! i | i <- idxs]) faces
  where
    toVx3 x = Vertex3 (x!!0) (x!!1) (x!!2)
