module Main where

import           Data.List (minimumBy, sort, transpose)
import           Data.Map  (Map, fromListWith, mapKeys, (!))
import qualified Data.Map  as Map
import           Data.Ord  (comparing)

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
relocate centroidsMap = mapKeys (\k -> centroid $ centroidsMap ! k) centroidsMap

-- mapKeys :: Ord k2 => (k1 -> k2) -> Map k1 a -> Map k2 a

-- ??
centroid :: [Point] -> Point
centroid = map avg . transpose
-- transpose :: [[a]] -> [[a]]

avg :: Fractional a => [a] -> a
avg xs = sum xs / fromIntegral (length xs)

-- Performs the k-means algorithm
kmeans :: [Point] -> [Point] -> [Point]
kmeans centroids points = undefined

main :: IO ()
main = do
  let points = [ [0, 0], [1, 0], [0,1], [1,1]
               , [7, 5], [9, 6], [8, 7] ]
  let centroids = kmeans (take 2 points) points
  print centroids
