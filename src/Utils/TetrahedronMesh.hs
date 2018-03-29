module Utils.TetrahedronMesh where
import qualified Data.IntMap.Strict           as IM
import           Data.List                    (union)
import           Data.Permute                 (elems, rank)
--import           Data.Tuple.Extra             (first, (&&&), (***), fst3, snd3, thd3)
import           Graphics.Rendering.OpenGL.GL (GLdouble)
-- import           Utils.TetrahedronFaces
-- import           Utils.CartesianPolar
--import           Utils.SphericalTriangle

allVertices :: [[GLdouble]]
allVertices =
    [ [ a,  a,  a],
      [ a,  a, -a],
      [ a, -a,  a],
      [-a, -a,  a],
      [-a,  a, -a],
      [-a,  a,  a],
      [ 0,  b, -c],
      [ 0, -b, -c],
      [ 0, -b,  c],
      [ c,  0, -b],
      [-c,  0, -b],
      [-c,  0,  b],
      [ b,  c,  0],
      [ b, -c,  0],
      [-b, -c,  0],
      [-b,  c,  0],
      [ 0,  b,  c],
      [ a, -a, -a],
      [ c,  0,  b],
      [-a, -a, -a] ]
  where
    phi = (1 + sqrt 5) / 2
    a = 1 / sqrt 3
    b = a / phi
    c = a * phi

tetra1Idxs :: [[Int]]
tetra1Idxs = [[16,13, 1],
              [16,10,13],
              [10, 1,13],
              [10,16, 1]]

fixIndices :: [[Double]] -> [[Int]] -> ([[Double]], [[Int]])
fixIndices allVertices faces = (newvertices, newfaces)
  where
  faceselems = foldr union [] faces
  l = length faceselems
  permute = elems $ rank l faceselems
  mapper = IM.fromList $ zip permute faceselems
  mapper' = IM.fromList $ zip faceselems permute
  newfaces = map (map ((IM.!) mapper')) faces
  newvertices = [allVertices !! (mapper IM.! i) | i <- [0 .. l-1]]


facesIdxs :: [[Int]]
facesIdxs = [ [1, 2, 3]
            , [0, 2, 3]
            , [0, 1, 3]
            , [0, 1, 2] ]

-- face1,face2,face3,face4 :: ([[Double]], [[Int]])
-- face1 :: [[Double]]
-- face1 = fst $ fixIndices allVertices tetra1Idxs
-- face2 = fixIndices allVertices [tetra1Idxs]
-- face3 = fixIndices allVertices [tetra1Idxs]
-- face4 = fixIndices allVertices [facesIdxs!!3]

-- angles = face1

-- face1Polar,face2Polar,face3Polar,face4Polar :: [((Double, Double, Double), [Int])]
-- face1Polar = zip (map cartesianToPolar' $ fst face1) (snd face1)
-- face2Polar = zip (map cartesianToPolar' $ fst face2) (snd face2)
-- face3Polar = zip (map cartesianToPolar' $ fst face3) (snd face3)
-- face4Polar = zip (map cartesianToPolar' $ fst face4) (snd face4)
--
-- angles1,angles2,angles3,angles4 :: [(Double,Double)]
-- angles1 = map (yz . fst) face1Polar where yz (_,y,z) = (y, z)
-- angles2 = map (yz . fst) face2Polar where yz (_,y,z) = (y, z)
-- angles3 = map (yz . fst) face3Polar where yz (_,y,z) = (y, z)
-- angles4 = map (yz . fst) face4Polar where yz (_,y,z) = (y, z)
