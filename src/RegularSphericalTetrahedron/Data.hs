module RegularSphericalTetrahedron.Data where

vertices :: [[Double]]
vertices =
  [ [-sqrt 2 / 3, -sqrt 2 / sqrt 3, -1/3]
  , [2*sqrt 2 / 3, 0, -1/3]
  , [- sqrt 2 / 3, sqrt 2 / sqrt 3, -1/3]
  , [0, 0, 1] ]

facesIdxs :: [[Int]]
facesIdxs = [[1,2,3],[3,2,0],[0,1,3],[2,1,0]]

vertices1 = [vertices !! i | i <- facesIdxs !! 0]
vertices2 = [vertices !! i | i <- facesIdxs !! 1]
vertices3 = [vertices !! i | i <- facesIdxs !! 2]
vertices4 = [vertices !! i | i <- facesIdxs !! 3]
