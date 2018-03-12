module AllCubes.TheCubes where
import           Data.List
import           Data.Tuple.Extra             (both)
import           Graphics.Rendering.OpenGL.GL (GLdouble, Vertex3 (..))
import           Math.Combinatorics.Graph

ncube :: Int -> [[Double]]
ncube n = concatMap (mapM (\x -> nub [x,-x])) [replicate n 1.0]

cube10 :: [[Double]] -- these are the vertices
cube10 = ncube 10

cube10' :: [Vertex3 Double]
cube10' = map toVx3 cube10
  where
  toVx3 x = Vertex3 (x!!0) (x!!1) (x!!2) -- nimporte quoi y'a pas que 3 coord !!

cubicalgraph :: Graph Int
cubicalgraph = q 10

edgesIdxs :: [(Int,Int)]
edgesIdxs = map toPair $ edges cubicalgraph
  where
  toPair x = (x!!0, x!!1)

cube10edges :: [(Vertex3 GLdouble, Vertex3 GLdouble)]
cube10edges = map (both (cube10' !!)) edgesIdxs




