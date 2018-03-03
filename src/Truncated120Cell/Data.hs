module Truncated120Cell.Data
  where
import Math.Combinat.Permutations
-- http://eusebeia.dyndns.org/4d/trunc120cell

signs :: Num a => [a] -> [[a]]
signs [] = [[]]
signs (x : xs)
  = let ps = signs xs
    in map (x :) ps ++ map ((-x) :) ps

signsAll :: Num a => [[a]] -> [[a]]
signsAll = concatMap signs

vertices1 :: [[Double]]
vertices1 =
     signsAll
     [[1, 3+4*phi, 3+4*phi, 3+4*phi]] ++
     [[3+4*phi, 1, 3+4*phi, 3+4*phi]] ++
     [[3+4*phi, 3+4*phi, 1, 3+4*phi]] ++
     [[3+4*phi, 3+4*phi, 3+4*phi, 1]]
     where phi = (1+sqrt 5) / 2

vertices2 :: [[Double]]
vertices2 =
  signsAll
  [[phi3, phi3, phi3, 5+6*phi]] ++
  [[phi3, phi3, 5+6*phi, phi3]] ++
  [[phi3, 5+6*phi, phi3, phi3]] ++
  [[5+6*phi, phi3, phi3, phi3]]
  where phi3 = phi*phi*phi
        phi = (1+sqrt 5) / 2

vertices3 :: [[Double]]
vertices3 =
  signsAll
  [[2*phi2, 2*phi2, 2*phi2, 2*phi4]] ++
  [[2*phi2, 2*phi2, 2*phi4, 2*phi2]] ++
  [[2*phi2, 2*phi4, 2*phi2, 2*phi2]] ++
  [[2*phi4, 2*phi2, 2*phi2, 2*phi2]]
  where phi2 = phi*phi
        phi4 = phi2*phi2
        phi = (1 + sqrt 5) / 2

vertices4 :: [[Double]]
vertices4 =
  signsAll $
  zipWith permuteList perms (replicate 24 [0, 1, 4+5*phi, phi5])
  where phi5 = phi*phi*phi*phi*phi
        phi = (1 + sqrt 5) / 2
        perms = filter isEvenPermutation (permutations 4)

--   vor Convexhull dodecaplex pour les permutations even

-- [ permuteList p [phi/2, 1/2, 1/2/phi, 0]    | p <- permutations4, isEvenPermutation p] ++


--
-- The coordinates of the truncated 120-cell, centered on the origin and having edge length 2, are all permutations of coordinate
-- and changes of sign of:
--
-- (1, 3+4φ, 3+4φ, 3+4φ)
-- (φ3, φ3, φ3, 5+6φ)
-- (2φ2, 2φ2, 2φ2, 2φ4)
-- as well as even permutations of coordinate and all changes of sign of:
--
-- (0, 1, 4+5φ, φ5)
-- (0, 1, 4+7φ, 1+3φ)
-- (0, φ2, 3φ3, 2+5φ)
-- (0, φ2, 5+6φ, φ4)
-- (0, 2φ, 2φ4, 2φ3)
-- (1, φ2, 4+7φ, 2φ2)
-- (1, φ3, 3φ3, 3+4φ)
-- (φ2, 2φ, 4+7φ, φ3)
-- (φ2, 2φ2, 4+5φ, 3+4φ)
-- (φ2, 2φ3, 2+5φ, 3+4φ)
-- (2φ, φ4, φ5, 3+4φ)
-- (φ3, 2φ2, φ5, 2+5φ)
-- (φ3, 1+3φ, 4+5φ, 2φ3)
-- (2φ2, 1+3φ, 3φ3, φ4)
