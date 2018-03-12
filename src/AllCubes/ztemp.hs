import           Data.List
import           Data.List.Index              (indexed)
import           Data.Tuple.Extra
import           Graphics.Rendering.OpenGL.GL (GLdouble, Vertex3 (..))
import           Math.Combinatorics.Graph
import           Text.Show.Pretty

ncube :: Int -> [[Int]]
ncube n = concatMap (mapM (\x -> nub [x,-x])) [replicate n 1]

cube10 :: [[Int]] -- these are the vertices
cube10 = ncube 5

thecube = map swap $ indexed cube10

cubicalgraph :: Graph Int
cubicalgraph = q 5

edgesIdxs :: [(Int,Int)]
edgesIdxs = map toPair $ edges cubicalgraph
  where
  toPair x = (x!!0, x!!1)


main :: IO ()
main = do
  putStrLn "the cube:"
  pPrint thecube
  putStrLn "edges:"
  pPrint edgesIdxs
