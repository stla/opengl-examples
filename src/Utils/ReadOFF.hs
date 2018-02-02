module Utils.ReadOFF
  where
import           Data.List
import           Text.Printf

readOFF :: FilePath -> IO ([[Float]], [[Int]])
readOFF file = do
  contents <- readFile file
  let contents' = lines contents
      nvertices = read $ head (words $ contents' !! 1) :: Int
      vcontents = take nvertices $ drop 2 contents'
      rows_vertices = map (bracketize . words) vcontents
      fcontents = drop (2 + nvertices) contents'
      rows_faces = map (bracketize . tail . words) fcontents
  return (read $ bracketize rows_vertices, read $ bracketize rows_faces)
  where bracketize = printf "[%s]" . intercalate ","
