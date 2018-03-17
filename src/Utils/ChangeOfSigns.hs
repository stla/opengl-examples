module Utils.ChangeOfSigns where
import           Data.List                  hiding (permutations)
import           Math.Combinat.Permutations



signsAll :: (Eq a, Num a) => [[a]] -> [[a]]
signsAll = concatMap signs
  where
    signs :: (Eq a, Num a) => [a] -> [[a]]
    signs = mapM (\x -> nub [x,-x])


vertices :: ([Double], Bool) -> [[Double]]
vertices (coords, allperms) =
  map (map (/ sqrt 128.9619)) $ signsAll $
  nub $ zipWith permuteList perms (replicate 24 coords)
  where perms = filter (if allperms then const True else isEvenPermutation) (permutations 4)

--example:
--allVertices :: [[Double]]
--allVertices = concatMap vertices
-- [ ([1, 1, 1+6*phi, 5+6*phi], True)
--  , ([1, 3, 3+6*phi, 3+6*phi], True)
--  , ([2, 2, 2+6*phi, 2*phi4], True)
