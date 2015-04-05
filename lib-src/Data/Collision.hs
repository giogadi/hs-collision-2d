module Data.Collision
       ( Point (..)
       , Polygon
       , inCollision
       , separatingAxisNormals
       , projectPolygonOntoAxisInterval
       ) where

import Linear.Metric
import Linear.V2

type Point = V2 Double

type Polygon = [Point]

separatingAxisNormals :: Polygon -> [V2 Double]
separatingAxisNormals points =
  let edges = zip points $ tail $ cycle points
  in  map (perp . uncurry (-)) edges

projectPolygonOntoAxisInterval :: Polygon -> V2 Double -> (Double, Double)
projectPolygonOntoAxisInterval points axis =
  let projections = map (dot axis) points
  in  (minimum projections, maximum projections)

intervalsOverlap :: (Double, Double) -> (Double, Double) -> Bool
intervalsOverlap (minA, maxA) (minB, maxB)
  | minA > maxB = False
  | minB > maxA = False
  | otherwise = True

axisNormalSeparatesPolygons :: Polygon -> Polygon -> V2 Double -> Bool
axisNormalSeparatesPolygons polyA polyB normal =
  let intervalA = projectPolygonOntoAxisInterval polyA normal
      intervalB = projectPolygonOntoAxisInterval polyB normal
  in  not $ intervalsOverlap intervalA intervalB

inCollision :: Polygon -> Polygon -> Bool
inCollision polyA polyB =
  let axisNormals = separatingAxisNormals polyA ++
                    separatingAxisNormals polyB
  in  all (not . axisNormalSeparatesPolygons polyA polyB) axisNormals
