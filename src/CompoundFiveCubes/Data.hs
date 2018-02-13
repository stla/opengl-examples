module CompoundFiveCubes.Data
  where
import           Data.List
import           Data.List.Split              (chunksOf)
import           Data.Tuple.Extra             (both)
import           Graphics.Rendering.OpenGL.GL (GLfloat, Vertex3 (..))

vertices :: [Vertex3 GLfloat]
vertices =
  map toVertex3 [ [-0.578318, 0.577033, 0.577629],
                  [0.000007, 0.355988, 0.934964],
                  [0.357433, 0.934549, -0.000640],
                  [-0.357417, 0.934604, -0.000607],
                  [0.000007, 0.356223, -0.934225],
                  [-0.001221, -0.356874, 0.934205],
                  [0.577138, -0.577774, 0.576836],
                  [0.934675, 0.000551, 0.356115],
                  [0.577284, 0.578876, 0.577071],
                  [0.576491, 0.577738, -0.577883],
                  [0.934061, -0.000498, -0.356982],
                  [0.355500, -0.934005, 0.000443],
                  [0.577138, -0.577774, -0.575951],
                  [-0.577269, -0.577774, 0.577072],
                  [-0.933900, -0.000588, 0.357867],
                  [-0.576567, 0.577795, -0.577883],
                  [-0.934047, -0.000497, -0.356982],
                  [-0.576386, -0.576581, -0.577883],
                  [-0.355665, -0.934117, 0.000442],
                  [0.001057, -0.356874, -0.933609] ]
  where
    toVertex3 x = Vertex3 (x!!0) (x!!1) (x!!2)

facesIdxs :: [[[Int]]]
facesIdxs = [ [ [3,1,7,9],
                [1,3,16,13],
                [9,19,16,3],
                [16,19,11,13],
                [1,13,11,7],
                [9,7,11,19] ],
              [ [2,4,16,0],
                [2,7,12,4],
                [2,0,5,7],
                [4,12,18,16],
                [0,16,18,5],
                [7,5,18,12] ],
              [ [3,8,10,4],
                [3,14,5,8],
                [8,5,11,10],
                [10,11,17,4],
                [4,17,14,3],
                [14,17,11,5] ],
              [ [8,6,12,9],
                [0,13,6,8],
                [15,17,13,0],
                [9,12,17,15],
                [15,0,8,9],
                [13,17,12,6] ],
              [ [2,10,19,15],
                [15,19,18,14],
                [15,14,1,2],
                [14,18,6,1],
                [1,6,10,2],
                [10,6,18,19] ]
            ]

edgesIdxs :: [[(Int,Int)]]
edgesIdxs = chunksOf 12 [ (3,1),
                          (1,7),
                          (7,9),
                          (9,3),
                          (3,16),
                          (16,13),
                          (13,1),
                          (9,19),
                          (19,16),
                          (19,11),
                          (11,13),
                          (11,7),
                          (2,4),
                          (4,16),
                          (16,0),
                          (0,2),
                          (2,7),
                          (7,12),
                          (12,4),
                          (0,5),
                          (5,7),
                          (12,18),
                          (18,16),
                          (18,5),
                          (3,8),
                          (8,10),
                          (10,4),
                          (4,3),
                          (3,14),
                          (14,5),
                          (5,8),
                          (5,11),
                          (11,10),
                          (11,17),
                          (17,4),
                          (17,14),
                          (8,6),
                          (6,12),
                          (12,9),
                          (9,8),
                          (0,13),
                          (13,6),
                          (8,0),
                          (15,17),
                          (17,13),
                          (0,15),
                          (12,17),
                          (15,9),
                          (2,10),
                          (10,19),
                          (19,15),
                          (15,2),
                          (19,18),
                          (18,14),
                          (14,15),
                          (14,1),
                          (1,2),
                          (18,6),
                          (6,1),
                          (6,10)
                        ]

faces :: [[[Vertex3 GLfloat]]]
faces = map (map (\idxs -> [vertices !! i | i <- idxs])) facesIdxs

edges :: [[(Vertex3 GLfloat, Vertex3 GLfloat)]]
edges = map (map (both (vertices !!))) edgesIdxs

vertices' :: [[Vertex3 GLfloat]]
vertices' =
  map (nub . concatMap (\idxs -> [vertices !! i | i <- idxs])) facesIdxs
