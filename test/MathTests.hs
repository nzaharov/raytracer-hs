module MathTests where

import Math.Utils
import Test.HUnit

inIntervalTest :: Test
inIntervalTest = TestCase $ do
  assertEqual "above" Nothing $ inInterval (1, 4) 5
  assertEqual "under" Nothing $ inInterval (1, 4) 0
  assertEqual "in" (Just 3) $ inInterval (1, 4) 3

smallerTest :: Test
smallerTest = TestCase $ do
  assertEqual "first" 1 $ smaller 1 2
  assertEqual "second" 2 $ smaller 3 2

clampTest :: Test
clampTest = TestCase $ do
  assertEqual "above" 4 $ clamp 5 (1, 4)
  assertEqual "under" 1 $ clamp 0 (1, 4)
  assertEqual "in" 3 $ clamp 3 (1, 4)

toRadiansTest :: Test
toRadiansTest = TestCase $ do
  assertEqual "pi" pi $ toRadians 180

mathTests :: Test
mathTests =
  TestList
    [ inIntervalTest,
      smallerTest,
      clampTest,
      toRadiansTest
    ]