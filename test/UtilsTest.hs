module UtilsTest where

import Test.HUnit
import Utils

chunkTest :: Test
chunkTest = TestCase $ do
  assertEqual "empty" ([] :: [[Int]]) $ chunks 5 ([] :: [Int])
  assertEqual "greater than len" [[1, 2, 3]] $ chunks 10 [1, 2, 3]
  assertEqual "normal" [[1, 2], [3, 4], [5]] $ chunks 2 [1,2,3,4,5]

utilsTests :: Test
utilsTests =
  TestList
    [ chunkTest
    ]