module GreatIcosahedron.Data
  where
import           Data.Tuple.Extra             (both)
import           Graphics.Rendering.OpenGL.GL (GLfloat, Vertex3 (..))

vertices :: [Vertex3 GLfloat]
vertices =
  map toVertex3 [ [0, 0, a], -- sqrt(phi+2)
                  [b, 0, -b/2], -- 1/sin(pi/5) 1/2/sin(pi/5)
                  [-c, 1, -b/2], -- cos(pi/5)/sin(pi/5)
                  [d, -phi, -b/2], -- (cos(pi/5)-0.5)/sin(pi/5)
                  [d, phi, -b/2],
                  [-c, -1, -b/2],
                  [-d, -phi, b/2],
                  [-d, phi, b/2],
                  [c, 1, b/2],
                  [-b, 0, b/2],
                  [c, -1, b/2],
                  [0, 0, -a]
                ]
  where
    toVertex3 x = Vertex3 (x!!0) (x!!1) (x!!2)
    phi = (1 + sqrt 5) / 2
    a = sqrt (phi+2)
    b = 1 / sin (pi/5)
    c = cos (pi/5) / sin (pi/5)
    d = (cos (pi/5) - 0.5) / sin (pi/5)

polygonsIdxs :: [[Int]]
polygonsIdxs =  [ [0,1,2],
                  [0,2,3],
                  [0,3,4],
                  [0,4,5],
                  [0,5,1],
                  [1,5,7],
                  [1,7,6],
                  [1,6,2],
                  [2,6,8],
                  [2,8,3],
                  [3,8,9],
                  [3,9,4],
                  [4,9,10],
                  [4,10,5],
                  [5,10,7],
                  [6,7,11],
                  [6,11,8],
                  [7,10,11],
                  [8,11,9],
                  [9,11,10]
                ]

edgesIdxs :: [(Int,Int)]
edgesIdxs = [  (0,1),
              (0,2),
              (0,3),
              (0,4),
              (0,5),
              (1,2),
              (1,5),
              (1,6),
              (1,7),
              (2,3),
              (2,6),
              (2,8),
              (3,4),
              (3,8),
              (3,9),
              (4,5),
              (4,9),
              (4,10),
              (5,7),
              (5,10),
              (6,7),
              (6,8),
              (6,11),
              (7,10),
              (7,11),
              (8,9),
              (8,11),
              (9,10),
              (9,11),
              (10,11)
            ]

polygons :: [[Vertex3 GLfloat]]
polygons = map (\idxs -> [vertices !! i | i <- idxs]) polygonsIdxs

edges :: [(Vertex3 GLfloat, Vertex3 GLfloat)]
edges = map (both (vertices !!)) edgesIdxs
