module CompoundFiveTetrahedra.Data
  where
import           Data.List
import           Data.Tuple.Extra             (both)
import           Graphics.Rendering.OpenGL.GL (GLfloat, Vertex3 (..))


vertices :: [Vertex3 GLfloat]
vertices =
  map toVertex3 [ [ a,  a,  a],
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
    toVertex3 x = Vertex3 (x!!0) (x!!1) (x!!2)
    phi = (1 + sqrt 5) / 2
    a = 1 / sqrt 3
    b = a / phi
    c = a * phi
  -- map toVertex3 [ [0.5773500, 0.5773500, 0.5773500], -- 1/sqrt(3)
  --                 [0.5773500, 0.5773500, -0.577350],
  --                 [0.5773500, -0.577350, 0.5773500],
  --                 [-0.577350, -0.577350, 0.5773500],
  --                 [-0.577350, 0.5773500, -0.577350],
  --                 [-0.577350, 0.5773500, 0.5773500],
  --                 [0.0000000, 0.3568220, -0.934173], -- 1/2/sqrt(3)/cos(Pi/5), cos(pi/5)/sin(pi/3)
  --                 [0.0000000, -0.356822, -0.934173], -- or 1/sqrt(3)/phi, phi/sqrt(3)
  --                 [0.0000000, -0.356822, 0.9341730],
  --                 [0.9341730, 0.0000000, -0.356822],
  --                 [-0.934173, 0.0000000, -0.356822],
  --                 [-0.934173, 0.0000000, 0.3568220],
  --                 [0.3568220, 0.9341730, 0.0000000],
  --                 [0.3568220, -0.934173, 0.0000000],
  --                 [-0.356822, -0.934173, 0.0000000],
  --                 [-0.356822, 0.9341730, 0.0000000],
  --                 [0.0000000, 0.3568220, 0.9341730],
  --                 [0.5773500, -0.577350, -0.577350],
  --                 [0.9341730, 0.0000000, 0.3568220],
  --                 [-0.577350, -0.577350, -0.577350]
  --               ]

tetra1Idxs :: [[Int]]
tetra1Idxs = [[16,13, 1],
              [16,10,13],
              [10, 1,13],
              [10,16, 1]
             ]

tetra2Idxs :: [[Int]]
tetra2Idxs = [[17, 0, 3],
              [17, 4, 0],
              [ 4,17, 3],
              [ 4, 3, 0]
             ]

tetra3Idxs :: [[Int]]
tetra3Idxs = [[18, 5,14],
              [18, 6, 5],
              [18,14, 6],
              [ 6,14, 5]
             ]

tetra4Idxs :: [[Int]]
tetra4Idxs = [[ 2,12,11],
              [ 2, 7,12],
              [ 7,11,12],
              [11, 7, 2]
             ]

tetra5Idxs :: [[Int]]
tetra5Idxs = [[19,15, 9],
              [19, 8,15],
              [ 8, 9,15],
              [19, 9, 8]
             ]

tetra1,tetra2,tetra3,tetra4,tetra5 :: [[Vertex3 GLfloat]]
tetra1 = map (\idxs -> [vertices !! i | i <- idxs]) tetra1Idxs
tetra2 = map (\idxs -> [vertices !! i | i <- idxs]) tetra2Idxs
tetra3 = map (\idxs -> [vertices !! i | i <- idxs]) tetra3Idxs
tetra4 = map (\idxs -> [vertices !! i | i <- idxs]) tetra4Idxs
tetra5 = map (\idxs -> [vertices !! i | i <- idxs]) tetra5Idxs

edgesIdxs :: [[(Int,Int)]]
edgesIdxs = [[(16,13),(13,1),(1,16),(16,10),(10,13),(10,1)],
             [(17,0),(0,3),(3,17),(17,4),(4,0),(3,4)],
             [(18,5),(5,14),(14,18),(18,6),(6,5),(14,6)],
             [(2,12),(12,11),(11,2),(2,7),(7,12),(7,11)],
             [(19,15),(15,9),(9,19),(19,8),(8,15),(8,9)]]

edges :: [[(Vertex3 GLfloat, Vertex3 GLfloat)]]
edges = map (map (both (vertices !!))) edgesIdxs

vertices1,vertices2,vertices3,vertices4,vertices5 :: [Vertex3 GLfloat]
vertices1 = nub $ concatMap (\idxs -> [vertices !! i | i <- idxs]) tetra1Idxs
vertices2 = nub $ concatMap (\idxs -> [vertices !! i | i <- idxs]) tetra2Idxs
vertices3 = nub $ concatMap (\idxs -> [vertices !! i | i <- idxs]) tetra3Idxs
vertices4 = nub $ concatMap (\idxs -> [vertices !! i | i <- idxs]) tetra4Idxs
vertices5 = nub $ concatMap (\idxs -> [vertices !! i | i <- idxs]) tetra5Idxs
