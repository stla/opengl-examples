module NonconvexPolyhedron.Data2
  where
import           Graphics.Rendering.OpenGL.GL (GLdouble, Vertex3 (..))
import           Utils.TetrahedronFaces
import           Linear (V3 (..), cross, (^-^), norm)
import           Data.List

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

tetrahedraFaces' :: [[[V3 Double]]]
tetrahedraFaces' = map (map (map vx3ToV3)) tetrahedraFaces
  where
    vx3ToV3 (Vertex3 x y z) = V3 x y z

triangleArea :: [V3 Double] -> Double
triangleArea vs = norm $ cross (vs!!1 ^-^ vs!!0) (vs!!2 ^-^ vs!!0)

tetrahedraFaces'' :: [[[Vertex3 Double]]]
tetrahedraFaces'' = map tail tetrahedraFaces

tetrahedraFacesIdxs' :: [[[Int]]]
tetrahedraFacesIdxs' = map tail tetrahedraFacesIdxs

allFaces :: [[Int]]
allFaces = concat tetrahedraFacesIdxs'
