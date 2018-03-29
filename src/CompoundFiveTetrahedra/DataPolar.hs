module CompoundFiveTetrahedra.DataPolar
  where
import           Data.List
import           Data.Tuple.Extra             (both)
import           Graphics.Rendering.OpenGL.GL (GLdouble, Normal3 (..),
                                               Vertex3 (..))
import           Utils.CartesianPolar
-- import           Utils.OpenGL                 (triangleNormal)
-- import Utils.SphericalTriangle

type TriDouble = (GLdouble, GLdouble, GLdouble)
type TriDoubleVx3 = (Vertex3 GLdouble, Vertex3 GLdouble, Vertex3 GLdouble)
triDouble2TriDoubleVx3 :: (TriDouble, TriDouble, TriDouble) -> TriDoubleVx3
triDouble2TriDoubleVx3 ((a, b, c), (a',b',c'), (a'',b'',c'')) =
  (toVx3 (a, b, c), toVx3 (a', b', c'), toVx3 (a'', b'', c''))
  where
    toVx3 :: (GLdouble, GLdouble, GLdouble) -> Vertex3 GLdouble
    toVx3 (x, y, z) = Vertex3 x y z

asTriplet :: [a] -> (a,a,a)
asTriplet x = (x!!0, x!!1, x!!2)

vertices :: [[GLdouble]]
vertices =
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

-- vx3ds :: [Vertex3 Double]
-- vx3ds = map toVx3 vertices
--   where
--   toVx3 :: [GLdouble] -> Vertex3 GLdouble
--   toVx3 [x, y, z] = Vertex3 x y z
--   toVx3 _ = undefined

--vx3dsPolar :: [(Double, Double, Double)]
-- vx3dsPolar = map cartesianToPolar' [[1,2,3]]


tetra1Idxs :: [[Int]]
tetra1Idxs = [[16,13, 1],
              [16,10,13],
              [10, 1,13],
              [10,16, 1]]

tetra1 :: [(Double, Double, Double)]
tetra1 = map cartesianToPolar' vertices

tetra2Idxs :: [[Int]]
tetra2Idxs = [[17, 0, 3],
              [17, 4, 0],
              [ 4,17, 3],
              [ 4, 3, 0]]
tetra2 :: [(Double, Double, Double)]
tetra2 = map cartesianToPolar' vertices

tetra3Idxs :: [[Int]]
tetra3Idxs = [[18, 5,14],
              [18, 6, 5],
              [18,14, 6],
              [ 6,14, 5]]

tetra4Idxs :: [[Int]]
tetra4Idxs = [[ 2,12,11],
              [ 2, 7,12],
              [ 7,11,12],
              [11, 7, 2]]

tetra5Idxs :: [[Int]]
tetra5Idxs = [[19,15, 9],
              [19, 8,15],
              [ 8, 9,15],
              [19, 9, 8]]


-- tr0 :: [(TriDoubleVx3, Normal3 GLdouble)]
-- tr0 = stMesh 6 1 (map (\i -> [vertices !! i]) idxs | idxs <- tetra1Idxs])
--
-- tetra1' :: [(TriDoubleVx3, Normal3 GLdouble)]
-- tetra1' =
--   concatMap (concatMap (\i ->
--                  (triDouble2TriDoubleVx3 (vertices'!!i, vertices'!!(i+1), vertices'!!(i+2)),
--                  n (i, i+1, i+2)))) tetra1Idxs
--   where
--     n (i, j, k) = triangleNormal $ triDouble2TriDoubleVx3 (vertices'!!i, vertices'!!j, vertices'!!k)
--
-- tetra2' :: [(TriDoubleVx3, Normal3 GLdouble)]
-- tetra2' =
--   concatMap (map (\i ->
--                  (triDouble2TriDoubleVx3 (vertices'!!i, vertices'!!(i+1), vertices'!!(i+2)),
--                  n (i, i+1, i+2)))) tetra2Idxs
--   where
--     n (i, j, k) = triangleNormal $ triDouble2TriDoubleVx3 (vertices'!!i, vertices'!!j, vertices'!!k)
--
-- tetra3' :: [(TriDoubleVx3, Normal3 GLdouble)]
-- tetra3' =
--   concatMap (map (\i ->
--                  (triDouble2TriDoubleVx3 (vertices'!!i, vertices'!!(i+1), vertices'!!(i+2)),
--                  n (i, i+1, i+2)))) tetra3Idxs
--   where
--     n (i, j, k) = triangleNormal $ triDouble2TriDoubleVx3 (vertices'!!i, vertices'!!j, vertices'!!k)
--
-- tetra4' :: [(TriDoubleVx3, Normal3 GLdouble)]
-- tetra4' =
--   concatMap (map (\i ->
--                  (triDouble2TriDoubleVx3 (vertices'!!i, vertices'!!(i+1), vertices'!!(i+2)),
--                  n (i, i+1, i+2)))) tetra4Idxs
--   where
--     n (i, j, k) = triangleNormal $ triDouble2TriDoubleVx3 (vertices'!!i, vertices'!!j, vertices'!!k)
--
-- tetra5' :: [(TriDoubleVx3, Normal3 GLdouble)]
-- tetra5' =
--   concatMap (map (\i ->
--                  (triDouble2TriDoubleVx3 (vertices'!!i, vertices'!!(i+1), vertices'!!(i+2)),
--                  n (i, i+1, i+2)))) tetra5Idxs
--   where
--     n (i, j, k) = triangleNormal $ triDouble2TriDoubleVx3 (vertices'!!i, vertices'!!j, vertices'!!k)
-- -- tetra1',tetra2',tetra3',tetra4',tetra5' :: [[GLdouble]]
-- tetra1' :: [[GLdouble]]
-- tetra1' = (cartesianToPolar . \idxs -> map asTriplet $ map (vertices' !!) idxs) tetra1Idxs
--   where
--     asTriplet x = (x!!0, x!!1, x!!2)
--
-- -- edgesIdxs :: [[(Int,Int)]]
-- -- edgesIdxs = [[(16,13),(13,1),(1,16),(16,10),(10,13),(10,1)],
-- --              [(17,0),(0,3),(3,17),(17,4),(4,0),(3,4)],
-- --              [(18,5),(5,14),(14,18),(18,6),(6,5),(14,6)],
-- --              [(2,12),(12,11),(11,2),(2,7),(7,12),(7,11)],
-- --              [(19,15),(15,9),(9,19),(19,8),(8,15),(8,9)]]
-- --
-- -- edges :: [[(Vertex3 GLdouble, Vertex3 GLdouble)]]
-- -- edges = map (map (both (vertices !!))) edgesIdxs
--
-- -- vertices1,vertices2,vertices3,vertices4,vertices5 :: [Vertex3 GLdouble]
-- -- vertices1 = nub $ concatMap (\idxs -> [(toVx3 $ (vertices' !! i)) | i <- idxs]) tetra1Idxs
-- -- vertices2 = nub $ concatMap (\idxs -> [(toVx3 $ (vertices' !! i)) | i <- idxs]) tetra2Idxs
-- -- vertices3 = nub $ concatMap (\idxs -> [(toVx3 $ (vertices' !! i)) | i <- idxs]) tetra3Idxs
-- -- vertices4 = nub $ concatMap (\idxs -> [(toVx3 $ (vertices' !! i)) | i <- idxs]) tetra4Idxs
-- -- vertices5 = nub $ concatMap (\idxs -> [(toVx3 $ (vertices' !! i)) | i <- idxs]) tetra5Idxs
-- --   where
-- --     toVx3 x = Vertex3 (x!!0) (x!!1) (x!!2)
