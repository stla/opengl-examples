module Pentaract.Data
  (cube5, cube5edges, edgesIdxs)
  where
import           Data.List
import           Data.Tuple.Extra             (both)
import           Graphics.Rendering.OpenGL.GL (GLdouble, Vertex3 (..))
import           Math.Combinatorics.Graph

ncube :: Int -> [[Double]]
ncube n = concatMap (mapM (\x -> nub [x,-x])) [replicate n 1.0]

cube5 :: [[Double]] -- these are the vertices
cube5 = map (map (/ sqrt 5)) (ncube 5)

cubicalgraph :: Graph Int
cubicalgraph = q 5

edgesIdxs :: [(Int,Int)]
edgesIdxs = map toPair $ edges cubicalgraph
  where
  toPair x = (x!!0, x!!1)

cube5edges :: [([Double], [Double])]
cube5edges = map (both (cube5 !!)) edgesIdxs
