module NonconvexPolyhedron.Data
  where
import           Graphics.Rendering.OpenGL.GL (GLdouble, Vertex3 (..))
import           Utils.TetrahedronFaces

vertices :: [[Double]]
vertices =  [ [ -x , -x , -x ] -- 0
            , [ -y , 0.0 , 0.0 ] -- 1
            , [ 0.0 , -y , 0.0 ] --  2
            , [ 0.0 , 0.0 , -y ] -- 3
            , [ -x , -x , x ] -- 4
            , [ 0.0 , 0.0 , y ] -- 5
            , [ -x , x , -x ] -- 6
            , [ 0.0 , y , 0.0 ] -- 7
            , [ -x , x , x ] -- 8
            , [ x , -x , -x ] -- 9
            , [ y , 0.0 , 0.0 ] -- 10
            , [ x , -x , x ] -- 11
            , [ x , x , -x ] -- 12
            , [ x , x , x ] -- 13
            ]
  where x = 2.1806973249
        y = 3.5617820682

tetrahedraIdxs :: [[Int]]
tetrahedraIdxs = [ [0,1,2,3]
                 , [4,1,2,5]
                 , [6,1,7,3]
                 , [8,1,7,5]
                 , [9,10,2,3]
                 , [11,10,2,5]
                 , [12,10,7,3]
                 , [13,10,7,5]
                 ]

tetrahedraFacesIdxs :: [[[Int]]]
tetrahedraFacesIdxs = map (tetraFacesIdxs vertices) tetrahedraIdxs

tetrahedraFaces :: [[[Vertex3 GLdouble]]]
tetrahedraFaces = map (map (\idxs -> [toVertex3 (vertices!!i) | i <- idxs])) tetrahedraFacesIdxs
  where
    toVertex3 x = Vertex3 (x!!0) (x!!1) (x!!2)


-- tetrahedra' :: [[Vertex3 GLdouble]]
-- tetrahedra' = map (map toVertex3) tetrahedra
--   where
--     toVertex3 l = Vertex3 (l!!0) (l!!1) (l!!2)
--
-- tetrahedraFaces :: [[[Vertex3 GLdouble]]]
-- tetrahedraFaces =
--   map (\ttrhdrn -> map (\idxs -> [ttrhdrn !! i | i <- idxs]) facesIdxs)
--       tetrahedra'
--
-- tetra1 = [0,1,2,3]
-- tetra2 = [4,1,2,5]
-- tetra3 = [6,1,7,3]
-- tetra4 = [8,1,7,5]
-- tetra5 = [9,10,2,3]
-- tetra6 = [11,10,2,5]
-- tetra7 = [12,10,7,3]
-- tetra8 = [13,10,7,5]
