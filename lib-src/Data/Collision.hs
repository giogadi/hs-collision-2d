module Data.Collision
       ( Point (..)
       , Polygon
       , inCollision
       , separatingAxisNormals
       , projectPolygonOntoAxisInterval
       , pushVector
       , polygonDistance
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

-- http://elancev.name/oliver/2D%20polygon.htm
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

pointSegmentDistance :: Point -> Point -> Point -> (Double, Double)
pointSegmentDistance p s1 s2 =
  let segVec = s2 - s1
      segQuadrance = quadrance segVec
  in  if segQuadrance == 0.0
      then (distance p s1, 0.0)
      else
        let bary = dot (p - s1) segVec / segQuadrance
        in  if bary < 0.0
            then (distance p s1, 0.0)
            else if bary > 1.0
                 then (distance p s2, 1.0)
                 else (distance p $ s1 + (bary *^ segVec), bary)

segmentSegmentDistance :: Point -> Point
                       -> Point -> Point
                       -> (Double, V2 Double)
segmentSegmentDistance u u' v v' =
  let pointSegments = [(u, (v, v')), (u', (v, v')),
                       (v, (u, u')), (v', (u, u'))]
      (ps, segments) = unzip pointSegments
      (s1s, s2s) = unzip segments
      distParamPairs = zipWith3 pointSegmentDistance ps s1s s2s
      distParamPtSegs = zip distParamPairs $ zip ps segments
      ((dist, param), (p, (s1, s2))) =
        minimumBy (comparing (fst . fst)) distParamPtSegs
  in  let distVec = p - (s1 + (param *^ (s2 - s1)))
      in  if (s1, s2) == (u, u')
          then (dist, distVec)
          else (dist, (-distVec))

polygonDistance :: Polygon -> Polygon -> (Double, V2 Double)
polygonDistance polyA polyB =
  let edgesA = zip polyA $ tail $ cycle polyA
      edgesB = zip polyB $ tail $ cycle polyB
      distVecs =
        [segmentSegmentDistance ea1 ea2 eb1 eb2 |
          (ea1, ea2) <- edgesA, (eb1, eb2) <- edgesB]
  in  minimumBy (comparing fst) distVecs
