module Truncated120Cell.Data
  where
import Math.Combinat.Permutations
import Data.List hiding (permutations)
import           Graphics.Rendering.OpenGL.GL (GLdouble, Vertex3 (..))

-- http://eusebeia.dyndns.org/4d/trunc120cell

signs :: (Eq a, Num a) => [a] -> [[a]]
signs [] = [[]]
signs (x : xs)
  = let ps = signs xs
    in nub $ map (x :) ps ++ map ((-x) :) ps

signsAll :: (Eq a, Num a) => [[a]] -> [[a]]
signsAll = concatMap signs

vs1 :: [[Double]]
vs1 =
     signsAll
     [[1, 3+4*phi, 3+4*phi, 3+4*phi]] ++
     [[3+4*phi, 1, 3+4*phi, 3+4*phi]] ++
     [[3+4*phi, 3+4*phi, 1, 3+4*phi]] ++
     [[3+4*phi, 3+4*phi, 3+4*phi, 1]]
     where phi = (1+sqrt 5) / 2

vs2 :: [[Double]]
vs2 =
  signsAll
  [[phi*phi*phi, phi*phi*phi, phi*phi*phi, 5+6*phi]] ++
  [[phi*phi*phi, phi*phi*phi, 5+6*phi, phi*phi*phi]] ++
  [[phi*phi*phi, 5+6*phi, phi*phi*phi, phi*phi*phi]] ++
  [[5+6*phi, phi*phi*phi, phi*phi*phi, phi*phi*phi]]
  where phi3 = phi*phi*phi
        phi = (1+sqrt 5) / 2

vs3 :: [[Double]]
vs3 =
  signsAll
  [[2*phi*phi, 2*phi*phi, 2*phi*phi, 2*phi4]] ++
  [[2*phi*phi, 2*phi*phi, 2*phi*phi*phi*phi, 2*phi*phi]] ++
  [[2*phi*phi, 2*phi*phi*phi*phi, 2*phi*phi, 2*phi*phi]] ++
  [[2*phi*phi*phi*phi, 2*phi*phi, 2*phi*phi, 2*phi*phi]]
  where phi2 = phi*phi
        phi4 = phi*phi*phi*phi
        phi = (1 + sqrt 5) / 2

vs4 :: [[Double]]
vs4 =
  signsAll $
  zipWith permuteList perms (replicate 12 [0, 1, 4+5*phi, phi5])
  where phi5 = phi*phi*phi*phi*phi
        phi = (1 + sqrt 5) / 2
        perms = filter isEvenPermutation (permutations 4)

vs5 :: [[Double]]
vs5 =
  signsAll $
  zipWith permuteList perms (replicate 12 [0, 1, 4+7*phi, 1+3*phi])
  where phi = (1 + sqrt 5) / 2
        perms = filter isEvenPermutation (permutations 4)

vertices6 :: [Double] -> [[Double]]
vertices6 coords =
  signsAll $
  zipWith permuteList perms (replicate 12 coords)
  where perms = filter isEvenPermutation (permutations 4)

vertices7 :: [Double] -> [[Double]]
vertices7 coords =
  signsAll $
  zipWith permuteList perms (replicate 12 coords)
  where perms = filter isEvenPermutation (permutations 4)

vertices8 :: [Double] -> [[Double]]
vertices8 coords =
  signsAll $
  zipWith permuteList perms (replicate 12 coords)
  where perms = filter isEvenPermutation (permutations 4)

vertices9 :: [Double] -> [[Double]]
vertices9 coords =
  signsAll $
  zipWith permuteList perms (replicate 12 coords)
  where perms = filter isEvenPermutation (permutations 4)

vertices10 :: [Double] -> [[Double]]
vertices10 coords =
  signsAll $
  zipWith permuteList perms (replicate 12 coords)
  where perms = filter isEvenPermutation (permutations 4)

vertices11 :: [Double] -> [[Double]]
vertices11 coords =
  signsAll $
  zipWith permuteList perms (replicate 12 coords)
  where perms = filter isEvenPermutation (permutations 4)

vertices12 :: [Double] -> [[Double]]
vertices12 coords =
  signsAll $
  zipWith permuteList perms (replicate 12 coords)
  where perms = filter isEvenPermutation (permutations 4)

vertices13 :: [Double] -> [[Double]]
vertices13 coords =
  signsAll $
  zipWith permuteList perms (replicate 12 coords)
  where perms = filter isEvenPermutation (permutations 4)

vertices14 :: [Double] -> [[Double]]
vertices14 coords =
  signsAll $
  zipWith permuteList perms (replicate 12 coords)
  where perms = filter isEvenPermutation (permutations 4)

vertices15 :: [Double] -> [[Double]]
vertices15 coords =
  signsAll $
  zipWith permuteList perms (replicate 12 coords)
  where perms = filter isEvenPermutation (permutations 4)

vertices16 :: [Double] -> [[Double]]
vertices16 coords =
  signsAll $
  zipWith permuteList perms (replicate 12 coords)
  where perms = filter isEvenPermutation (permutations 4)

vertices17 :: [Double] -> [[Double]]
vertices17 coords =
  signsAll $
  zipWith permuteList perms (replicate 12 coords)
  where perms = filter isEvenPermutation (permutations 4)

phi :: Double
phi = (1+ sqrt 5) / 2

vs6 = vertices6 [0, phi*phi, 3*phi*phi*phi, 2+5*phi]
vs7 = vertices7 [0, phi*phi, 5+6*phi, phi*phi*phi*phi]
vs8 = vertices8 [0, 2*phi, 2*phi*phi*phi*phi, 2*phi*phi*phi]
vs9 = vertices9 [1, phi*phi, 4+7*phi, 2*phi*phi]
vs10 = vertices10 [1, phi*phi*phi, 3*phi*phi*phi, 3+4*phi]
vs11 = vertices11 [phi*phi, 2*phi, 4+7*phi, phi*phi*phi]
vs12 = vertices12 [phi*phi, 2*phi*phi, 4+5*phi, 3+4*phi]
vs13 = vertices13 [phi*phi, 2*phi*phi*phi, 2+5*phi, 3+4*phi]
vs14 = vertices14 [2*phi, phi*phi*phi*phi, phi*phi*phi*phi*phi, 3+4*phi]
vs15 = vertices15 [phi*phi*phi, 2*phi*phi, phi*phi*phi*phi*phi, 2+5*phi]
vs16 = vertices16 [phi*phi*phi, 1+3*phi, 4+5*phi, 2*phi*phi*phi]
vs17 = vertices17 [2*phi*phi, 1+3*phi, 3*phi*phi*phi, phi*phi*phi*phi]

allVertices = map (map (/ 16.43667)) $ vs1 ++ vs2 ++ vs3 ++ vs4 ++ vs5 ++ vs6 ++ vs7 ++ vs8 ++ vs9 ++ vs10 ++ vs11 ++ vs12 ++ vs13 ++ vs14 ++ vs15 ++ vs16 ++ vs17

-- The coordinates of the truncated 120-cell, centered on the origin and having edge length 2, are all permutations of coordinate
-- and changes of sign of:
--
-- (1, 3+4*phi, 3+4*phi, 3+4*phi)
-- (phi*phi*phi, phi*phi*phi, phi*phi*phi, 5+6*phi)
-- (2*phi*phi, 2*phi*phi, 2*phi*phi, 2*phi4)
-- as well as even permutations of coordinate and all changes of sign of:
--
-- (0, 1, 4+5*phi, phi*phi*phi*phi*phi)
-- (0, 1, 4+7*phi, 1+3*phi)

-- [0, phi*phi, 3*phi*phi*phi, 2+5*phi]
-- [0, phi*phi, 5+6*phi, phi*phi*phi*phi]
-- [0, 2*phi, 2*phi*phi*phi*phi, 2*phi*phi*phi]
-- [1, phi*phi, 4+7*phi, 2*phi*phi]
-- [1, phi*phi*phi, 3*phi*phi*phi, 3+4*phi]
-- [phi*phi, 2*phi, 4+7*phi, phi*phi*phi]
-- [phi*phi, 2*phi*phi, 4+5*phi, 3+4*phi]
-- [phi*phi, 2*phi*phi*phi, 2+5*phi, 3+4*phi]
-- [2*phi, phi*phi*phi*phi, phi*phi*phi*phi*phi, 3+4*phi]
-- [phi*phi*phi, 2*phi*phi, phi*phi*phi*phi*phi, 2+5*phi]
-- [phi*phi*phi, 1+3*phi, 4+5*phi, 2*phi*phi*phi]
-- [2*phi*phi, 1+3*phi, 3*phi*phi*phi, phi*phi*phi*phi]
