import Data.Maybe
import Data.Monoid
import Linear.Metric
import Linear.V2
import Linear.Vector
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Data.Collision

separatingAxisNormalsTest :: Assertion
separatingAxisNormalsTest =
  let rect = [V2 0 0, V2 1 0, V2 1 1, V2 0 1] :: Polygon
      axisNormals = [V2 0 (-1), V2 1 0, V2 0 1, V2 (-1) 0] :: [V2 Double]
  in  separatingAxisNormals rect @?= axisNormals

projectAxisTest :: Assertion
projectAxisTest =
  let rect = [V2 1 0, V2 1 1, V2 1 2, V2 0 1] :: [V2 Double]
      axis = V2 1 0
      interval = (0.0, 1.0)
  in  projectPolygonOntoAxisInterval rect axis @?= interval

rectsCollisionFreeTest :: Assertion
rectsCollisionFreeTest =
  let rect1 = [V2 0 0, V2 1 0, V2 1 1, V2 0 1] :: Polygon
      rect2 = [V2 2 0, V2 3 0, V2 3 1, V2 2 1] :: Polygon
  in  inCollision rect1 rect2 @?= False

rectsCollisionTest :: Assertion
rectsCollisionTest =
  let rect1 = [V2 0 0, V2 1 0, V2 1 1, V2 0 1] :: Polygon
      rect2 = map (+ V2 0.5 0) rect1 :: Polygon
  in  inCollision rect1 rect2 @?= True

rectsCollision2Test :: Assertion
rectsCollision2Test =
  let rect1 = [V2 0 0, V2 1 0, V2 1 1, V2 0 1] :: Polygon
      rect2 = [V2 1 0, V2 0 1, V2 (-1) 0, V2 0 (-1)] :: Polygon
      rect2Moved = map (+ V2 1.5 0.5) rect2
  in inCollision rect1 rect2Moved @?= True

rectsCollisionFree2Test :: Assertion
rectsCollisionFree2Test =
  let rect1 = [V2 0 0, V2 1 0, V2 1 1, V2 0 1] :: Polygon
      rect2 = [V2 1 0, V2 0 1, V2 (-1) 0, V2 0 (-1)] :: Polygon
      rect2Moved = map (+ V2 2.001 0.5) rect2
  in inCollision rect1 rect2Moved @?= False

rectsPushTest :: Assertion
rectsPushTest =
  let rect1 = [V2 0 0, V2 1 0, V2 1 1, V2 0 1] :: Polygon
      rect2 = map (+ V2 0.8 0) rect1
      pushV = V2 0.2 0
      errorV = fromJust (pushVector rect1 rect2) - pushV
  in  quadrance errorV < 0.00001 @?= True

rectsPush2Test :: Assertion
rectsPush2Test =
  let rect1 = [V2 0 0, V2 1 0, V2 1 1, V2 0 1] :: Polygon
      rect2 = [V2 1 0, V2 0 1, V2 (-1) 0, V2 0 (-1)] :: Polygon
      rect2Moved = map (+ V2 1 (-0.5)) rect2
      pushV = V2 1 (-1) ^* (0.5 * 0.5)
  in  pushVector rect1 rect2Moved @?= Just pushV

main = defaultMain tests
  where tests = [ testCase "boxCollision" rectsCollisionTest
                , testCase "bolCollision2" rectsCollision2Test
                , testCase "boxCollisionFree" rectsCollisionFreeTest
                , testCase "boxCollisionFree2" rectsCollisionFree2Test
                , testCase "sepAxisNormals" separatingAxisNormalsTest
                , testCase "projectAxis" projectAxisTest
                , testCase "boxPush" rectsPushTest
                , testCase "boxPush2" rectsPush2Test
                ]
