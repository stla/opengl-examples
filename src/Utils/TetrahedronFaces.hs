module Utils.TetrahedronFaces
  where
import           Linear (V3 (..), cross, dot, (^-^))

-- vertices :: [[Double]]
-- vertices =  [ [ -2.1806973249 , -2.1806973249 , -2.1806973249 ] -- 0
--             , [ -3.5617820682 , 0.0 , 0.0 ] -- 1
--             , [ 0.0 , -3.5617820682 , 0.0 ] --  2
--             , [ 0.0 , 0.0 , -3.5617820682 ] -- 3
--             , [ -2.1806973249 , -2.1806973249 , 2.1806973249 ] -- 4
--             , [ 0.0 , 0.0 , 3.5617820682 ] -- 5
--             , [ -2.1806973249 , 2.1806973249 , -2.1806973249 ] -- 6
--             , [ 0.0 , 3.5617820682 , 0.0 ] -- 7
--             , [ -2.1806973249 , 2.1806973249 , 2.1806973249 ] -- 8
--             , [ 2.1806973249 , -2.1806973249 , -2.1806973249 ] -- 9
--             , [ 3.5617820682 , 0.0 , 0.0 ] -- 10
--             , [ 2.1806973249 , -2.1806973249 , 2.1806973249 ] -- 11
--             , [ 2.1806973249 , 2.1806973249 , -2.1806973249 ] -- 12
--             , [ 2.1806973249 , 2.1806973249 , 2.1806973249 ] -- 13
--             ]

-- tetra1Idxs,tetra2Idxs,tetra3Idxs,tetra4Idxs,tetra5Idxs,tetra6Idxs,tetra7Idxs,tetra8Idxs :: [Int]
-- tetra1Idxs = [0,1,2,3]
-- tetra2Idxs = [4,1,2,5]
-- tetra3Idxs = [6,1,7,3]
-- tetra4Idxs = [8,1,7,5]
-- tetra5Idxs = [9,10,2,3]
-- tetra6Idxs = [11,10,2,5]
-- tetra7Idxs = [12,10,7,3]
-- tetra8Idxs = [13,10,7,5]

tetraFacesIdxs :: [[Double]] -> [Int] -> [[Int]]
tetraFacesIdxs vertices tetraIdxs =
  map (\i -> if tetradotProducts!!i
                then tetraFacesIdxs'!!i
                else reverse (tetraFacesIdxs'!!i)) [0 .. 3]
  where
    facesIdxs = [ [1, 2, 3]
                , [0, 2, 3]
                , [0, 1, 3]
                , [0, 1, 2] ]
    tetraFacesIdxs' :: [[Int]]
    tetraFacesIdxs' = map (\idxs -> [tetraIdxs !! i | i <- idxs]) facesIdxs
    vertices' :: [V3 Double]
    vertices' = map dbl2V3 vertices
      where dbl2V3 x = V3 (x!!0) (x!!1) (x!!2)
    tetra :: [V3 Double]
    tetra = map (vertices' !!) tetraIdxs
    tetraFaces :: [[V3 Double]]
    tetraFaces = map (\ijk -> [vertices' !! l | l <- ijk]) tetraFacesIdxs'
    tetraFacesNormals :: [V3 Double]
    tetraFacesNormals = map normal tetraFaces
      where normal abc = cross (abc!!1 ^-^ abc!!0) (abc!!2 ^-^ abc!!0)
    tetradotProducts :: [Bool]
    tetradotProducts = map (<0) (zipWith dot vecs tetraFacesNormals)
      where vecs = map (\i -> tetra!!i ^-^ head (tetraFaces!!i)) [0 .. 3]

-- facesIdxs :: [[Int]]
-- facesIdxs = [ [1, 2, 3] -- 0
--             , [0, 2, 3] -- 1
--             , [0, 1, 3] -- 2
--             , [0, 1, 2] -- 3
--             ]
--
-- tetra1FacesIdxs :: [[Int]]
-- tetra1FacesIdxs = map (\idxs -> [tetra1Idxs !! i | i <- idxs]) facesIdxs
--
-- vertices' :: [V3 Double]
-- vertices' = map dbl2V3 vertices
--   where
--     dbl2V3 x = V3 (x!!0) (x!!1) (x!!2)
--
-- tetra1 :: [V3 Double]
-- tetra1 = map (vertices' !!) tetra1Idxs
--
-- tetra1Faces :: [[V3 Double]] -- [(V3 Double, V3 Double, V3 Double)]
-- tetra1Faces = map (\ijk -> [vertices' !! l | l <- ijk]) tetra1FacesIdxs
--
-- tetra1FacesNormals :: [V3 Double]
-- tetra1FacesNormals = map normal tetra1Faces
--   where
--     normal abc = cross (abc!!1 ^-^ abc!!0) (abc!!2 ^-^ abc!!0)
--
-- tetra1dotProducts :: [Bool]
-- tetra1dotProducts = map (<0) (zipWith dot vecs tetra1FacesNormals)
--   where
--     vecs = map (\i -> tetra1!!i ^-^ head (tetra1Faces!!i)) [0 .. 3]
--
-- tetra1FacesIdxs' :: [[Int]]
-- tetra1FacesIdxs' =
--   map (\i -> if tetra1dotProducts!!i
--                 then tetra1FacesIdxs!!i
--                 else reverse (tetra1FacesIdxs!!i)) [0 .. 3]
