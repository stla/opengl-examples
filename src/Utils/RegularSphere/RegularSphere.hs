module Utils.RegularSphere.RegularSphere
  where
import Data.List
import Data.Function (on)
import Data.Tuple.Extra (second, (***), first)
import qualified Data.Map.Strict
import           Data.Map.Strict (Map)

regularSphere :: Int -> [Double] -> Double -> ([[Double]], [[(Int,Int)]]) -- [[(0,1),(0,2),(0,3)], [(1,1),(1,2),(1,3)], [(2,1),(2,2),(2,3)], [(3,1),(3,2),(3,3)]]
regularSphere n center rho =
  (zipWith (s2c rho) theta phi,
    groupBy ((==) `on` fst) [(i,j) | i <- [0 .. n-1], j <- [1 .. n-1]])
  where
    gridtheta = [frac i n | i <- [0 .. n-1]]
    theta = map (*(2*pi)) gridtheta
    gridphi = [frac i n | i <- [1 .. n-1]]
    phi = map (*pi) gridphi
    frac :: Int -> Int -> Double
    frac p q = realToFrac p / realToFrac q
    s2c :: Double -> Double -> Double -> [Double]
    s2c r th ph = [r * cos th * sin ph + center!!0, r * sin th * sin ph + center!!1, r * cos ph + center!!2]

-- x = [(0,1),(0,2),(0,3)]
--
-- z = [fst (x!!0), snd (x!!0), fst (x!!1), snd (x!!1)]
--
-- w = map (\i -> [fst (x!!i), snd (x!!i), fst (x!!(i+1)), snd (x!!(i+1))]) [0 .. length x -2]
--
regularSphere' :: Int -> [Double] -> Double -> ([[Double]], [[[Int]]])
regularSphere' n center rho = (vertices, idxs')
  where
  (vertices, xx) = regularSphere n center rho
  idxs' = map (\j ->  map (\i -> [fst (xx!!j!!i), snd (xx!!j!!i), fst (xx!!j!!(i+1)), snd (xx!!j!!(i+1))]) [0 .. length (xx!!j) -2]) [0..length xx-1]


x = [(0,1),(0,2),(0,3)]

type RectangleIdxs = (Int,Int,Int,Int)

toRectangle :: [(Int,Int)] -> [RectangleIdxs]
toRectangle x = zipWith (\e1 e2 -> (fst e1, fst e2, snd e2, snd e1)) (init x) (tail x)
--  toRectangle [(0,1),(0,2),(0,3)]
--  [(0,1,0,2),(0,2,0,3)]

toRectangle' :: [Int] -> [[RectangleIdxs]]
toRectangle' x = [toRectangle [(x!!0,x!!1)]]


regularSphere'' :: Int -> [Double] -> Double -> ([[Double]], [[RectangleIdxs]])
regularSphere'' n center rho = second (map toRectangle) (regularSphere n center rho)


-- toMap :: [ [ [Int] -> [[RectangleIdxs]] ] ]


regularSphere''' :: Int -> [Double] -> Double -> ([[Double]], [[[Int] -> [[RectangleIdxs]]]])
regularSphere''' n center rho = (id *** (map (\x -> [toRectangle']))) (regularSphere' n center rho)
