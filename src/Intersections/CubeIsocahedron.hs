module Intersections.CubeIsocahedron
  where
import           Data.List                    (union)
import           Data.Tuple.Extra             (both)
import           Graphics.Rendering.OpenGL.GL (GLfloat, Vertex3 (..))

vertices :: [Vertex3 GLfloat]
vertices =  [ Vertex3 (-1.0) (-1.0) (-0.6180339887498949)
            , Vertex3 (-1.0) (-0.6180339887498949) (-1.0)
            , Vertex3 (-1.0) 0.6180339887498949 (-1.0)
            , Vertex3 (-1.0) 1.0 (-0.6180339887498949)
            , Vertex3 (-1.0) 1.0 0.6180339887498949
            , Vertex3 (-1.0) 0.6180339887498949 1.0
            , Vertex3 (-1.0) (-0.6180339887498949) 1.0
            , Vertex3 (-1.0) (-1.0) 0.6180339887498949
            , Vertex3 (-0.6180339887498949) (-1.0) 1.0
            , Vertex3 0.6180339887498949 (-1.0) 1.0
            , Vertex3 1.0 (-1.0) 0.6180339887498949
            , Vertex3 1.0 (-1.0) (-0.6180339887498949)
            , Vertex3 0.6180339887498949 (-1.0) (-1.0)
            , Vertex3 (-0.6180339887498949) (-1.0) (-1.0)
            , Vertex3 1.0 (-0.6180339887498949) (-1.0)
            , Vertex3 1.0 0.6180339887498949 (-1.0)
            , Vertex3 0.6180339887498949 1.0 (-1.0)
            , Vertex3 (-0.6180339887498949) 1.0 (-1.0)
            , Vertex3 1.0 1.0 (-0.6180339887498949)
            , Vertex3 1.0 1.0 0.6180339887498949
            , Vertex3 0.6180339887498949 1.0 1.0
            , Vertex3 (-0.6180339887498949) 1.0 1.0
            , Vertex3 1.0 0.6180339887498949 1.0
            , Vertex3 1.0 (-0.6180339887498949) 1.0
            ]

polygonsIdxs :: [[Int]]
polygonsIdxs =  [ [ 1 , 0 , 7 , 6 , 5 , 4 , 3 , 2 ]
                , [ 7 , 0 , 13 , 12 , 11 , 10 , 9 , 8 ]
                , [ 13 , 0 , 1 ]
                , [ 13 , 1 , 2 , 17 , 16 , 15 , 14 , 12 ]
                , [ 17 , 2 , 3 ]
                , [ 17 , 3 , 4 , 21 , 20 , 19 , 18 , 16 ]
                , [ 4 , 5 , 21 ]
                , [ 5 , 6 , 8 , 9 , 23 , 22 , 20 , 21 ]
                , [ 6 , 7 , 8 ]
                , [ 23 , 9 , 10 ]
                , [ 10 , 11 , 14 , 15 , 18 , 19 , 22 , 23 ]
                , [ 11 , 12 , 14 ]
                , [ 15 , 16 , 18 ]
                , [ 19 , 20 , 22 ]
                ]

edgesIdxs :: [(Int,Int)]
edgesIdxs = foldr (union . (\idxs -> (last idxs, head idxs) : zip (init idxs) (tail idxs))) [] polygonsIdxs

polygons :: [[Vertex3 GLfloat]]
polygons = map (\idxs -> [vertices !! i | i <- idxs]) polygonsIdxs

edges :: [(Vertex3 GLfloat, Vertex3 GLfloat)]
edges = map (both (vertices !!)) edgesIdxs
