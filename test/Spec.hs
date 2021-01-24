import CameraTest (cameraTests)
import MathTests (mathTests)
import Test.HUnit
import UtilsTest (utilsTests)

tests :: Test
tests =
  TestList
    [ utilsTests,
      mathTests,
      cameraTests
    ]

main :: IO Counts
main = runTestTT tests
