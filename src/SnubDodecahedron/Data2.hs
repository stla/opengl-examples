module SnubDodecahedron.Data2 where
import           Data.List                    hiding (permutations)
import           Data.Tuple.Extra             (both)
import           Graphics.Rendering.OpenGL.GL
import           Math.Combinat.Permutations

signsAll2 :: (Eq a, Num a) => [[a]] -> [[a]]
signsAll2 = concatMap signs
  where
    signs :: (Eq a, Num a) => [a] -> [[a]]
    signs [x,y,z] = nub [[x,y,-z], [x,-y,z], [-x,y,z]]


vertices :: ([Float], Bool) -> [[Float]]
vertices (coords, allperms) =
  -- map (map (/ sqrt 128.9619)) $
  signsAll2 $
  nub $ zipWith permuteList perms (replicate 12 coords)
  where perms = filter (if allperms then const True else isEvenPermutation) (permutations 3)

--example:
--allVertices :: [[Float]]
--allVertices = concatMap vertices
-- [ ([1, 1, 1+6*phi, 5+6*phi], True)
--  , ([1, 3, 3+6*phi, 3+6*phi], True)
--  , ([2, 2, 2+6*phi, 2*phi4], True)

snubDodecahedron :: [[Float]]
snubDodecahedron = concatMap vertices
  [ ([2*alpha, 2, 2*beta], False)
   , ([alpha + beta/phi + phi, alpha*phi + beta + 1/phi, alpha/phi + beta*phi-1], False)
   , ([alpha + beta/phi - phi, alpha*phi - beta + 1/phi, alpha/phi + beta*phi+1], False)
   , ([alpha/phi + beta*phi+1, alpha + beta/phi - phi, alpha*phi + beta - 1/phi], False)
   , ([alpha/phi + beta*phi-1, alpha - beta/phi - phi, alpha*phi + beta - 1/phi], False) ]
  where
    alpha = xi - 1/xi
    beta = xi*phi + phi*phi + xi/phi
    xi = 1.7155615
    phi = (1 + sqrt 5)/2


snubDodecahedron' :: [Vertex3 Float]
snubDodecahedron' = map toVx3 snubDodecahedron
  where
    toVx3 x = Vertex3 (x!!0) (x!!1) (x!!2)


facetsIdxs :: [[Int]]
facetsIdxs =
    [ [ 18 , 22 , 26 ]
    , [ 4 , 10 , 22 ]
    , [ 6 , 7 , 25 ]
    , [ 28 , 19 , 16 ]
    , [ 19 , 28 , 37 ]
    , [ 22 , 31 , 40 ]
    , [ 31 , 22 , 10 ]
    , [ 10 , 16 , 31 ]
    , [ 6 , 25 , 27 ]
    , [ 36 , 27 , 25 ]
    , [ 18 , 27 , 36 ]
    , [ 40 , 38 , 22 ]
    , [ 38 , 29 , 8 ]
    , [ 8 , 26 , 38 ]
    , [ 38 , 26 , 22 ]
    , [ 10 , 13 , 16 ]
    , [ 7 , 13 , 25 ]
    , [ 28 , 16 , 13 ]
    , [ 28 , 13 , 7 ]
    , [ 37 , 28 , 24 ]
    , [ 24 , 7 , 6 ]
    , [ 7 , 24 , 28 ]
    , [ 19 , 37 , 41 ]
    , [ 27 , 12 , 6 ]
    , [ 6 , 12 , 24 ]
    , [ 40 , 31 , 20 ]
    , [ 20 , 38 , 40 ]
    , [ 20 , 29 , 38 ]
    , [ 19 , 2 , 1 ]
    , [ 1 , 16 , 19 ]
    , [ 31 , 16 , 1 ]
    , [ 1 , 2 , 20 ]
    , [ 1 , 20 , 31 ]
    , [ 3 , 9 , 21 ]
    , [ 30 , 21 , 9 ]
    , [ 43 , 36 , 25 ]
    , [ 18 , 36 , 43 ]
    , [ 4 , 22 , 43 ]
    , [ 43 , 22 , 18 ]
    , [ 33 , 24 , 12 ]
    , [ 3 , 23 , 33 ]
    , [ 33 , 9 , 3 ]
    , [ 33 , 12 , 9 ]
    , [ 42 , 41 , 37 ]
    , [ 23 , 41 , 42 ]
    , [ 42 , 33 , 23 ]
    , [ 42 , 37 , 24 ]
    , [ 24 , 33 , 42 ]
    , [ 9 , 12 , 15 ]
    , [ 27 , 15 , 12 ]
    , [ 9 , 15 , 30 ]
    , [ 30 , 15 , 0 ]
    , [ 0 , 15 , 18 ]
    , [ 27 , 18 , 15 ]
    , [ 21 , 30 , 39 ]
    , [ 21 , 39 , 44 ]
    , [ 44 , 39 , 26 ]
    , [ 39 , 30 , 0 ]
    , [ 0 , 18 , 39 ]
    , [ 18 , 26 , 39 ]
    , [ 34 , 25 , 13 ]
    , [ 25 , 34 , 43 ]
    , [ 43 , 34 , 4 ]
    , [ 34 , 10 , 4 ]
    , [ 34 , 13 , 10 ]
    , [ 23 , 32 , 41 ]
    , [ 32 , 23 , 11 ]
    , [ 41 , 32 , 19 ]
    , [ 2 , 19 , 32 ]
    , [ 44 , 35 , 21 ]
    , [ 26 , 35 , 44 ]
    , [ 5 , 11 , 23 ]
    , [ 35 , 11 , 5 ]
    , [ 3 , 5 , 23 ]
    , [ 21 , 5 , 3 ]
    , [ 5 , 21 , 35 ]
    , [ 35 , 14 , 11 ]
    , [ 29 , 14 , 8 ]
    , [ 8 , 14 , 26 ]
    , [ 35 , 26 , 14 ]
    , [ 29 , 17 , 14 ]
    , [ 11 , 14 , 17 ]
    , [ 11 , 17 , 32 ]
    , [ 32 , 17 , 2 ]
    , [ 2 , 17 , 20 ]
    , [ 29 , 20 , 17 ]
    ]

edgesIdxs :: [(Int,Int)]
edgesIdxs =
      [ ( 0 , 15 )
      , ( 0 , 18 )
      , ( 0 , 30 )
      , ( 0 , 39 )
      , ( 1 , 2 )
      , ( 1 , 16 )
      , ( 1 , 19 )
      , ( 1 , 20 )
      , ( 1 , 31 )
      , ( 2 , 17 )
      , ( 2 , 19 )
      , ( 2 , 20 )
      , ( 2 , 32 )
      , ( 3 , 5 )
      , ( 3 , 9 )
      , ( 3 , 21 )
      , ( 3 , 23 )
      , ( 3 , 33 )
      , ( 4 , 10 )
      , ( 4 , 22 )
      , ( 4 , 34 )
      , ( 4 , 43 )
      , ( 5 , 11 )
      , ( 5 , 21 )
      , ( 5 , 23 )
      , ( 5 , 35 )
      , ( 6 , 7 )
      , ( 6 , 12 )
      , ( 6 , 24 )
      , ( 6 , 25 )
      , ( 6 , 27 )
      , ( 7 , 13 )
      , ( 7 , 24 )
      , ( 7 , 25 )
      , ( 7 , 28 )
      , ( 8 , 14 )
      , ( 8 , 26 )
      , ( 8 , 29 )
      , ( 8 , 38 )
      , ( 9 , 12 )
      , ( 9 , 15 )
      , ( 9 , 21 )
      , ( 9 , 30 )
      , ( 9 , 33 )
      , ( 10 , 13 )
      , ( 10 , 16 )
      , ( 10 , 22 )
      , ( 10 , 31 )
      , ( 10 , 34 )
      , ( 11 , 14 )
      , ( 11 , 17 )
      , ( 11 , 23 )
      , ( 11 , 32 )
      , ( 11 , 35 )
      , ( 12 , 15 )
      , ( 12 , 24 )
      , ( 12 , 27 )
      , ( 12 , 33 )
      , ( 13 , 16 )
      , ( 13 , 25 )
      , ( 13 , 28 )
      , ( 13 , 34 )
      , ( 14 , 17 )
      , ( 14 , 26 )
      , ( 14 , 29 )
      , ( 14 , 35 )
      , ( 15 , 18 )
      , ( 15 , 27 )
      , ( 15 , 30 )
      , ( 16 , 19 )
      , ( 16 , 28 )
      , ( 16 , 31 )
      , ( 17 , 20 )
      , ( 17 , 29 )
      , ( 17 , 32 )
      , ( 18 , 22 )
      , ( 18 , 26 )
      , ( 18 , 27 )
      , ( 18 , 36 )
      , ( 18 , 39 )
      , ( 18 , 43 )
      , ( 19 , 28 )
      , ( 19 , 32 )
      , ( 19 , 37 )
      , ( 19 , 41 )
      , ( 20 , 29 )
      , ( 20 , 31 )
      , ( 20 , 38 )
      , ( 20 , 40 )
      , ( 21 , 30 )
      , ( 21 , 35 )
      , ( 21 , 39 )
      , ( 21 , 44 )
      , ( 22 , 26 )
      , ( 22 , 31 )
      , ( 22 , 38 )
      , ( 22 , 40 )
      , ( 22 , 43 )
      , ( 23 , 32 )
      , ( 23 , 33 )
      , ( 23 , 41 )
      , ( 23 , 42 )
      , ( 24 , 28 )
      , ( 24 , 33 )
      , ( 24 , 37 )
      , ( 24 , 42 )
      , ( 25 , 27 )
      , ( 25 , 34 )
      , ( 25 , 36 )
      , ( 25 , 43 )
      , ( 26 , 35 )
      , ( 26 , 38 )
      , ( 26 , 39 )
      , ( 26 , 44 )
      , ( 27 , 36 )
      , ( 28 , 37 )
      , ( 29 , 38 )
      , ( 30 , 39 )
      , ( 31 , 40 )
      , ( 32 , 41 )
      , ( 33 , 42 )
      , ( 34 , 43 )
      , ( 35 , 44 )
      , ( 36 , 43 )
      , ( 37 , 41 )
      , ( 37 , 42 )
      , ( 38 , 40 )
      , ( 39 , 44 )
      , ( 41 , 42 )
      ]

edges :: [(Vertex3 Float, Vertex3 Float)]
edges = map (both (snubDodecahedron' !!)) edgesIdxs

facets :: [[Vertex3 Float]]
facets = map (\idxs -> [snubDodecahedron' !! i | i <- idxs]) facetsIdxs
