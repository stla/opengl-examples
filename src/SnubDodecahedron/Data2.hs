module SnubDodecahedron.Data2 where
import           Data.List                    hiding (permutations)
import           Data.Tuple.Extra             (both)
import           Graphics.Rendering.OpenGL.GL
import           Math.Combinat.Permutations

signsAll2 :: (Eq a, Num a) => [[a]] -> [[a]]
signsAll2 = concatMap signs
  where
    signs :: (Eq a, Num a) => [a] -> [[a]]
    signs [x,y,z] = nub [[x,y,-z], [x,-y,z], [-x,y,z], [-x,-y,-z]]


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
  , ([alpha + beta/phi + phi, -alpha*phi + beta + 1/phi, alpha/phi + beta*phi-1], False)
  , ([alpha + beta/phi - phi, alpha*phi - beta + 1/phi, alpha/phi + beta*phi+1], False)
  , ([-alpha/phi + beta*phi+1, -alpha + beta/phi - phi, alpha*phi + beta - 1/phi], False)
  , ([-alpha/phi + beta*phi-1, alpha - beta/phi - phi, alpha*phi + beta + 1/phi], False) ]
  where
    alpha = xi - 1/xi
    beta = xi*phi + phi*phi + phi/xi
    xi = 1.7155615
    phi = (1 + sqrt 5)/2


snubDodecahedron' :: [Vertex3 Float]
snubDodecahedron' = map toVx3 snubDodecahedron
  where
    toVx3 x = Vertex3 (x!!0) (x!!1) (x!!2)


facetsIdxs :: [[Int]]
facetsIdxs =
  [ [ 45 , 17 , 5 ]
  , [ 45 , 21 , 17 ]
  , [ 8 , 33 , 36 ]
  , [ 29 , 7 , 5 ]
  , [ 43 , 15 , 3 ]
  , [ 43 , 19 , 15 ]
  , [ 13 , 21 , 37 ]
  , [ 13 , 17 , 21 ]
  , [ 49 , 37 , 32 ]
  , [ 56 , 49 , 32 ]
  , [ 40 , 12 , 0 ]
  , [ 40 , 16 , 12 ]
  , [ 44 , 16 , 4 ]
  , [ 24 , 3 , 0 ]
  , [ 3 , 24 , 43 ]
  , [ 55 , 43 , 24 ]
  , [ 5 , 7 , 31 ]
  , [ 5 , 31 , 45 ]
  , [ 57 , 45 , 31 ]
  , [ 31 , 55 , 57 ]
  , [ 7 , 19 , 43 , 55 , 31 ]
  , [ 47 , 19 , 7 ]
  , [ 7 , 29 , 47 ]
  , [ 56 , 44 , 30 ]
  , [ 4 , 30 , 44 ]
  , [ 38 , 22 , 10 ]
  , [ 46 , 22 , 18 ]
  , [ 26 , 50 , 53 ]
  , [ 1 , 2 , 26 ]
  , [ 25 , 2 , 1 ]
  , [ 1 , 13 , 37 , 49 , 25 ]
  , [ 37 , 21 , 9 ]
  , [ 9 , 32 , 37 ]
  , [ 32 , 9 , 8 ]
  , [ 8 , 9 , 33 ]
  , [ 9 , 21 , 45 , 57 , 33 ]
  , [ 12 , 20 , 36 ]
  , [ 36 , 20 , 8 ]
  , [ 12 , 16 , 20 ]
  , [ 44 , 20 , 16 ]
  , [ 8 , 20 , 44 , 56 , 32 ]
  , [ 57 , 48 , 33 ]
  , [ 48 , 36 , 33 ]
  , [ 57 , 55 , 48 ]
  , [ 24 , 48 , 55 ]
  , [ 0 , 12 , 36 , 48 , 24 ]
  , [ 0 , 3 , 27 ]
  , [ 0 , 27 , 40 ]
  , [ 52 , 40 , 27 ]
  , [ 27 , 51 , 52 ]
  , [ 50 , 38 , 35 ]
  , [ 10 , 35 , 38 ]
  , [ 25 , 49 , 54 ]
  , [ 56 , 54 , 49 ]
  , [ 30 , 54 , 56 ]
  , [ 4 , 16 , 40 , 52 , 28 ]
  , [ 58 , 51 , 34 ]
  , [ 58 , 52 , 51 ]
  , [ 28 , 52 , 58 ]
  , [ 58 , 46 , 28 ]
  , [ 10 , 22 , 46 , 58 , 34 ]
  , [ 14 , 22 , 38 ]
  , [ 14 , 18 , 22 ]
  , [ 2 , 14 , 38 , 50 , 26 ]
  , [ 53 , 41 , 26 ]
  , [ 1 , 26 , 41 ]
  , [ 41 , 13 , 1 ]
  , [ 41 , 17 , 13 ]
  , [ 5 , 17 , 41 , 53 , 29 ]
  , [ 59 , 53 , 50 ]
  , [ 59 , 50 , 35 ]
  , [ 29 , 53 , 59 ]
  , [ 59 , 47 , 29 ]
  , [ 15 , 19 , 23 ]
  , [ 47 , 23 , 19 ]
  , [ 42 , 14 , 2 ]
  , [ 42 , 18 , 14 ]
  , [ 2 , 25 , 42 ]
  , [ 54 , 42 , 25 ]
  , [ 46 , 18 , 6 ]
  , [ 6 , 28 , 46 ]
  , [ 28 , 6 , 4 ]
  , [ 4 , 6 , 30 ]
  , [ 6 , 18 , 42 , 54 , 30 ]
  , [ 34 , 11 , 10 ]
  , [ 10 , 11 , 35 ]
  , [ 11 , 23 , 47 , 59 , 35 ]
  , [ 39 , 23 , 11 ]
  , [ 15 , 23 , 39 ]
  , [ 51 , 39 , 34 ]
  , [ 11 , 34 , 39 ]
  , [ 3 , 15 , 39 , 51 , 27 ]
  ]

edgesIdxs :: [(Int,Int)]
edgesIdxs =
  [ ( 0 , 3 )
  , ( 0 , 12 )
  , ( 0 , 24 )
  , ( 0 , 27 )
  , ( 0 , 40 )
  , ( 1 , 2 )
  , ( 1 , 13 )
  , ( 1 , 25 )
  , ( 1 , 26 )
  , ( 1 , 41 )
  , ( 2 , 14 )
  , ( 2 , 25 )
  , ( 2 , 26 )
  , ( 2 , 42 )
  , ( 3 , 15 )
  , ( 3 , 24 )
  , ( 3 , 27 )
  , ( 3 , 43 )
  , ( 4 , 6 )
  , ( 4 , 16 )
  , ( 4 , 28 )
  , ( 4 , 30 )
  , ( 4 , 44 )
  , ( 5 , 7 )
  , ( 5 , 17 )
  , ( 5 , 29 )
  , ( 5 , 31 )
  , ( 5 , 45 )
  , ( 6 , 18 )
  , ( 6 , 28 )
  , ( 6 , 30 )
  , ( 6 , 46 )
  , ( 7 , 19 )
  , ( 7 , 29 )
  , ( 7 , 31 )
  , ( 7 , 47 )
  , ( 8 , 9 )
  , ( 8 , 20 )
  , ( 8 , 32 )
  , ( 8 , 33 )
  , ( 8 , 36 )
  , ( 9 , 21 )
  , ( 9 , 32 )
  , ( 9 , 33 )
  , ( 9 , 37 )
  , ( 10 , 11 )
  , ( 10 , 22 )
  , ( 10 , 34 )
  , ( 10 , 35 )
  , ( 10 , 38 )
  , ( 11 , 23 )
  , ( 11 , 34 )
  , ( 11 , 35 )
  , ( 11 , 39 )
  , ( 12 , 16 )
  , ( 12 , 20 )
  , ( 12 , 36 )
  , ( 12 , 40 )
  , ( 13 , 17 )
  , ( 13 , 21 )
  , ( 13 , 37 )
  , ( 13 , 41 )
  , ( 14 , 18 )
  , ( 14 , 22 )
  , ( 14 , 38 )
  , ( 14 , 42 )
  , ( 15 , 19 )
  , ( 15 , 23 )
  , ( 15 , 39 )
  , ( 15 , 43 )
  , ( 16 , 20 )
  , ( 16 , 40 )
  , ( 16 , 44 )
  , ( 17 , 21 )
  , ( 17 , 41 )
  , ( 17 , 45 )
  , ( 18 , 22 )
  , ( 18 , 42 )
  , ( 18 , 46 )
  , ( 19 , 23 )
  , ( 19 , 43 )
  , ( 19 , 47 )
  , ( 20 , 36 )
  , ( 20 , 44 )
  , ( 21 , 37 )
  , ( 21 , 45 )
  , ( 22 , 38 )
  , ( 22 , 46 )
  , ( 23 , 39 )
  , ( 23 , 47 )
  , ( 24 , 43 )
  , ( 24 , 48 )
  , ( 24 , 55 )
  , ( 25 , 42 )
  , ( 25 , 49 )
  , ( 25 , 54 )
  , ( 26 , 41 )
  , ( 26 , 50 )
  , ( 26 , 53 )
  , ( 27 , 40 )
  , ( 27 , 51 )
  , ( 27 , 52 )
  , ( 28 , 46 )
  , ( 28 , 52 )
  , ( 28 , 58 )
  , ( 29 , 47 )
  , ( 29 , 53 )
  , ( 29 , 59 )
  , ( 30 , 44 )
  , ( 30 , 54 )
  , ( 30 , 56 )
  , ( 31 , 45 )
  , ( 31 , 55 )
  , ( 31 , 57 )
  , ( 32 , 37 )
  , ( 32 , 49 )
  , ( 32 , 56 )
  , ( 33 , 36 )
  , ( 33 , 48 )
  , ( 33 , 57 )
  , ( 34 , 39 )
  , ( 34 , 51 )
  , ( 34 , 58 )
  , ( 35 , 38 )
  , ( 35 , 50 )
  , ( 35 , 59 )
  , ( 36 , 48 )
  , ( 37 , 49 )
  , ( 38 , 50 )
  , ( 39 , 51 )
  , ( 40 , 52 )
  , ( 41 , 53 )
  , ( 42 , 54 )
  , ( 43 , 55 )
  , ( 44 , 56 )
  , ( 45 , 57 )
  , ( 46 , 58 )
  , ( 47 , 59 )
  , ( 48 , 55 )
  , ( 48 , 57 )
  , ( 49 , 54 )
  , ( 49 , 56 )
  , ( 50 , 53 )
  , ( 50 , 59 )
  , ( 51 , 52 )
  , ( 51 , 58 )
  , ( 52 , 58 )
  , ( 53 , 59 )
  , ( 54 , 56 )
  , ( 55 , 57 )
  ]

edges :: [(Vertex3 Float, Vertex3 Float)]
edges = map (both (snubDodecahedron' !!)) edgesIdxs

facets :: [[Vertex3 Float]]
facets = map (\idxs -> [snubDodecahedron' !! i | i <- idxs]) facetsIdxs
