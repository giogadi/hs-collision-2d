module Data.Collision
       ( Point (..)
       , Polygon
       , inCollision
       , separatingAxisNormals
       , projectPolygonOntoAxisInterval
       , pushVector
       ) where

import Data.List
import Data.Maybe
import Data.Ord
import Linear.Metric
import Linear.V2
import Linear.Vector

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

intervalsOverlap :: (Double, Double) -> (Double, Double) -> Maybe Double
intervalsOverlap (minA, maxA) (minB, maxB)
  | minA > maxB = Nothing
  | minB > maxA = Nothing
  | otherwise = Just $ min maxA maxB - max minA minB

polygonOverlapAlongAxisNormal :: Polygon -> Polygon -> V2 Double -> Maybe Double
polygonOverlapAlongAxisNormal polyA polyB normal =
  let intervalA = projectPolygonOntoAxisInterval polyA normal
      intervalB = projectPolygonOntoAxisInterval polyB normal
  in  intervalsOverlap intervalA intervalB

axisNormalSeparatesPolygons :: Polygon -> Polygon -> V2 Double -> Bool
axisNormalSeparatesPolygons polyA polyB normal =
  isNothing $ polygonOverlapAlongAxisNormal polyA polyB normal

pushVector :: Polygon -> Polygon -> Maybe (V2 Double)
pushVector polyA polyB =
  let axisNormals = separatingAxisNormals polyA ++
                    -- Reverse B's normals so that pushVector is
                    -- always intended for moving B out of collision
                    map negate (separatingAxisNormals polyB)
      maybeOverlaps =
        map (polygonOverlapAlongAxisNormal polyA polyB) axisNormals
  in  if any isNothing maybeOverlaps
      then Nothing
      else
        let overlaps = map fromJust maybeOverlaps
            axisQuadrances = map quadrance axisNormals
            -- We scale by (1 / axisQuadrances) because both overlaps
            -- and axisNormals are scaled by the axis normal lengths,
            -- so we divide their produce twice by the lengths to get
            -- the normalized vectors.
            pushVecs =
              zipWith (^/) (zipWith (*^) overlaps axisNormals) axisQuadrances
        in  Just $ minimumBy (comparing quadrance) pushVecs

inCollision :: Polygon -> Polygon -> Bool
inCollision polyA polyB = isJust $ pushVector polyA polyB
