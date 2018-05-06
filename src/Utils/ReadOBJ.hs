module Utils.ReadOBJ
  where
import           Data.List

readOBJ :: FilePath -> IO ([[Float]], [[Int]])
readOBJ file = do
  contents <- readFile file
  let contents' = lines contents
      vlines = filter (\x -> x!!0 == 'v') contents'
      vvlines = map (drop 2) vlines
      flines = filter (\x -> x!!0 == 'f') contents'
      fflines = map (drop 2) flines
      vertices = map (map read . words) vvlines
      faces = map (map (\x -> read x :: Int) . words) fflines
  return (vertices, faces)
