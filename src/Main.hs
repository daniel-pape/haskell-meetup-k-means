module Main where

import           Data.List  (minimumBy, sort, transpose)
import           Data.Map   (Map, assocs, elems, fromListWith, keys, mapKeys,
                             (!))
import           Data.Maybe
import           Data.Ord   (comparing)

-- A `Point` is a just a `List` of `Double` entries (a Euclidean vector):
type Point = [Double]

sqr :: Num a => a -> a
sqr a = a * a

-- Computes the Euclidean distance between two points `a` and `b`.
dist :: Point -> Point -> Double
dist a b = sqrt . sum $ zipWith (\a' b' -> sqr $ a' - b') a b


-- Returns a `Map`, which assigns each point from `points` to the closest
-- centroid (taken from `centroids`).
assign :: [Point] -> [Point] -> Map Point [Point]
assign centroids points = fromListWith (++) [(pointWithMinDist p centroids, [p]) | p <- points ]

-- fromList :: Ord k => [(k, a)] -> Map k a
-- minimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
-- comparing :: Ord a => (b -> a) -> b -> b -> Ordering
-- fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> Map k a

pointWithMinDist :: Point -> [Point] -> Point
pointWithMinDist x = minimumBy (comparing $ dist x)


-- Replaces the centroid (key) in `centroidsMap` with the centroids
-- computed from the points (value) in `centroidsMap`, thus returning.
relocate :: Map Point [Point] -> Map Point [Point]
relocate centroidsMap = mapKeys (\k -> relocate' k (centroidsMap ! k)) centroidsMap
  where
  relocate' :: Point -> [Point] -> Point
  relocate' c ps = fromMaybe c (centroid ps)

-- mapKeys :: Ord k2 => (k1 -> k2) -> Map k1 a -> Map k2 a

centroid :: [Point] -> Maybe Point
centroid [] = Nothing
centroid ps = Just . map avg . transpose $ ps
-- transpose :: [[a]] -> [[a]]

avg :: Fractional a => [a] -> a
avg xs = sum xs / fromIntegral (length xs)

kMeansStep :: ([Point], [Point]) -> ([Point], [Point])
kMeansStep (cs, ps) = (keys map, concat . elems $ map)
  where
  map :: Map Point [Point]
  map = relocate $ assign cs ps

kMeansGenerator :: Int -> [Point] -> [([Point], [Point])]
kMeansGenerator k points = generator (centroids, points) kMeansStep
  where
  centroids = take k points

-- Performs the k-means algorithm
kMeans :: Int -> [Point] -> [Point]
kMeans k points = undefined

generator :: a -> (a -> a) -> [a]
generator s f = s : generator (f s) f

points = [ [0, 0], [1, 0], [0, 1], [1, 1] , [7, 5], [9, 6], [8, 7] ]

showAssign :: (Show a, Show b) => (a, b) -> String
showAssign (x, y) = show x ++ " -> " ++ show y

showMap :: (Show a, Show b) => Map a b -> String
showMap = unwords . map showAssign . assocs

main :: IO ()
main = putStr $ unlines $ map showMap $ map (uncurry assign) $ take 3 $ kMeansGenerator 2 points
