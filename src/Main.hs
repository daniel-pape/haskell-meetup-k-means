module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (minimumBy, sort, transpose)
import Data.Ord (comparing)

-- A `Point` is a just a `List` of `Double` entries (a Euclidean vector):
type Point = [Double]

-- Computes the Euclidean distance between two points `a` and `b`.
dist :: Point -> Point -> Double
dist a b = undefined

-- Returns a `Map`, which assigns each point from `points` to the closest
-- centroid (taken from `centroids`).
assign :: [Point] -> [Point] -> Map Point [Point]
assign centroids points = undefined

-- Replaces the centroid (key) in `centroidsMap` with the centroids
-- computed from the points (value) in `centroidsMap`, thus returning.
relocate :: Map Point [Point] -> Map Point [Point]
relocate centroidsMap = undefined

-- Performs the k-means algorithm
kmeans :: [Point] -> [Point] -> [Point]
kmeans centroids points = undefined

main :: IO ()
main = do
  let points = [ [0, 0], [1, 0], [0,1], [1,1]
               , [7, 5], [9, 6], [8, 7] ]
  let centroids = kmeans (take 2 points) points
  print centroids
